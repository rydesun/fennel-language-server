mod config;
mod helper;
mod view;

use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    sync::{Arc, RwLock},
};

use dashmap::DashMap;
use fennel_parser::{models, Ast};
use helper::*;
use ropey::Rope;
use tower_lsp::{
    jsonrpc::{Error, Result},
    lsp_types::*,
};

#[derive(Debug)]
struct Backend {
    client: tower_lsp::Client,
    config: Arc<RwLock<config::Configuration>>,
    doc_map: DashMap<Url, Rope>,
    ast_map: DashMap<Url, Ast>,
    workspace_map: DashMap<Url, String>,
    // publish those before saving
    on_save_or_open_errors: DashMap<Url, Vec<fennel_parser::Error>>,
}

#[tower_lsp::async_trait]
impl tower_lsp::LanguageServer for Backend {
    async fn initialize(
        &self,
        params: InitializeParams,
    ) -> Result<InitializeResult> {
        if let Some(folders) = params.workspace_folders {
            folders.into_iter().for_each(|folder| {
                let mut uri = folder.uri;
                if let Ok(mut segments) = uri.path_segments_mut() {
                    segments.push("/");
                } else {
                    return;
                }
                self.workspace_map.insert(uri.clone(), folder.name);
            });
        }

        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: env!("CARGO_PKG_NAME").into(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(
                        WorkspaceFoldersServerCapabilities {
                            supported: Some(true),
                            change_notifications: None,
                        },
                    ),
                    file_operations: None,
                }),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".into(), ":".into()]),
                    ..Default::default()
                }),
                code_action_provider: Some(
                    CodeActionProviderCapability::Simple(true),
                ),
                ..Default::default()
            },
        })
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let ast = self.ast_map.get(&uri).ok_or_else(Error::invalid_request)?;
        let doc = self.doc_map.get(&uri).ok_or_else(Error::invalid_request)?;
        let offset = position_to_byte_offset(&doc, position)?;

        match ast.definition(offset) {
            Some(fennel_parser::Definition::Symbol(symbol, _)) => {
                let range = lsp_range(&doc, symbol.token.range)?;
                match symbol.value.kind {
                    models::ValueKind::Require(Some(file)) => {
                        self.find_file(&uri, file).map_or_else(
                            || {
                                Ok(Some(GotoDefinitionResponse::Scalar(
                                    Location::new(uri.clone(), range),
                                )))
                            },
                            |new_uri| {
                                Ok(Some(GotoDefinitionResponse::Array(vec![
                                    Location::new(uri.clone(), range),
                                    Location::new(new_uri, lsp_range_head()),
                                ])))
                            },
                        )
                    }
                    _ => Ok(Some(GotoDefinitionResponse::Scalar(
                        Location::new(uri, range),
                    ))),
                }
            }
            Some(fennel_parser::Definition::FileSymbol(path, symbol)) => {
                let range = lsp_range(&doc, symbol.token.range)?;
                let res = self.find_file(&uri, path).map(|uri| {
                    GotoDefinitionResponse::Scalar(Location::new(uri, range))
                });
                Ok(res)
            }
            Some(fennel_parser::Definition::File(path)) => {
                let res = self.find_file(&uri, path).map(|uri| {
                    GotoDefinitionResponse::Scalar(Location::new(
                        uri,
                        lsp_range_head(),
                    ))
                });
                Ok(res)
            }
            None => Ok(None),
        }
    }

    async fn references(
        &self,
        params: ReferenceParams,
    ) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let ast = self.ast_map.get(&uri).ok_or_else(Error::invalid_request)?;
        let doc = self.doc_map.get(&uri).ok_or_else(Error::invalid_request)?;
        let offset = position_to_byte_offset(&doc, position)?;

        let references = ast.reference(offset);
        if references.is_none() {
            return Ok(None);
        }
        let references = references.unwrap();
        if references.is_empty() {
            return Err(Error::request_cancelled());
        }
        let mut locations = Vec::with_capacity(references.len());
        for reference in references {
            let range = lsp_range(&doc, reference)?;
            locations.push(Location::new(uri.clone(), range));
        }
        Ok(Some(locations))
    }

    async fn rename(
        &self,
        params: RenameParams,
    ) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let ast = self.ast_map.get(&uri).ok_or_else(Error::invalid_request)?;
        let doc = self.doc_map.get(&uri).ok_or_else(Error::invalid_request)?;
        let offset = position_to_byte_offset(&doc, position)?;

        if !ast.validate_name(&params.new_name) {
            return Err(Error::invalid_params("Illegal identifier name"));
        }
        let ranges = ast.reference(offset).ok_or_else(|| {
            Error::invalid_params("No references found at position")
        })?;
        if ranges.is_empty() {
            return Ok(None);
        }

        let mut changes = Vec::with_capacity(ranges.len());
        for range in ranges {
            let range = lsp_range(&doc, range)?;
            changes.push(TextEdit::new(range, params.new_name.clone()))
        }
        let mut map = HashMap::new();
        map.insert(uri, changes);
        Ok(Some(WorkspaceEdit::new(map)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let ast = self.ast_map.get(&uri).ok_or_else(Error::invalid_request)?;
        let doc = self.doc_map.get(&uri).ok_or_else(Error::invalid_request)?;
        let offset = position_to_byte_offset(&doc, position)?;

        let symbol = match ast.definition(offset) {
            Some(fennel_parser::Definition::Symbol(symbol, _)) => symbol,
            _ => return Ok(None),
        };
        let range = lsp_range(&doc, symbol.token.range)?;
        let text = symbol.token.text;
        let scope_kind = view::scope_kind(symbol.scope.kind);
        let value_kind = view::value_kind(&symbol.value.kind);

        let header_text = format!(
            "{} {}{}{}",
            scope_kind,
            text,
            if value_kind.is_empty() {
                "".to_owned()
            } else {
                " : ".to_owned() + value_kind
            },
            if let Some(literal) = ast.literal_value(symbol.value) {
                let prefix =
                    if literal.contains('\n') { " =\n" } else { " = " };
                prefix.to_owned() + &literal
            } else {
                "".to_owned()
            },
        );
        let body_text = if symbol.scope.kind == models::ScopeKind::Func {
            ast.docstring(symbol.token.range)
        } else {
            None
        };

        let header = MarkedString::LanguageString(LanguageString {
            language: "fennel".into(),
            value: header_text,
        });
        let contents = if let Some(body_text) = body_text {
            HoverContents::Array(vec![
                header,
                MarkedString::LanguageString(LanguageString {
                    language: "markdown".into(),
                    value: body_text,
                }),
            ])
        } else {
            HoverContents::Scalar(header)
        };
        Ok(Some(Hover { contents, range: Some(range) }))
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let ast = self.ast_map.get(&uri).ok_or_else(Error::invalid_request)?;
        let doc = self.doc_map.get(&uri).ok_or_else(Error::invalid_request)?;
        let offset = position_to_byte_offset(&doc, position)?;

        let trigger = params.context.and_then(|ctx| ctx.trigger_character);
        let (symbols, globals) = ast.completion(offset, trigger);
        let symbols = symbols.map(|symbol| CompletionItem {
            label: symbol.token.text.clone(),
            insert_text: Some(symbol.token.text.clone()),
            kind: Some(view::completion_scope_kind(symbol.scope.kind)),
            detail: Some(symbol.token.text.clone()),
            ..Default::default()
        });
        let globals = globals.into_iter().flat_map(|(kind, vec)| {
            vec.into_iter().map(move |word| CompletionItem {
                label: word.to_owned(),
                insert_text: Some(word.to_owned()),
                kind: Some(view::completion_value_kind(kind)),
                detail: Some(word.to_owned()),
                ..Default::default()
            })
        });
        let completions = symbols.chain(globals).collect();
        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn code_action(
        &self,
        params: CodeActionParams,
    ) -> Result<Option<CodeActionResponse>> {
        let uri = params.text_document.uri;
        let ast = self.ast_map.get(&uri).ok_or_else(Error::invalid_request)?;
        let doc = self.doc_map.get(&uri).ok_or_else(Error::invalid_request)?;
        let offset = position_to_byte_offset(&doc, params.range.start)?;

        let actions = ast.hint_action(offset);
        let res = actions.iter().filter_map(|(range, action)| {
            let range = lsp_range(&doc, *range).ok()?;
            let action = match action {
                fennel_parser::Action::ConvertToColonString(s) => {
                    let mut map = HashMap::new();
                    map.insert(uri.clone(), vec![TextEdit::new(
                        range,
                        s.to_owned(),
                    )]);
                    CodeActionOrCommand::CodeAction(CodeAction {
                        title: "Convert string to start with a colon"
                            .to_string(),
                        kind: Some(CodeActionKind::REFACTOR),
                        edit: Some(WorkspaceEdit::new(map)),
                        ..Default::default()
                    })
                }
                fennel_parser::Action::ConvertToQuoteString(s) => {
                    let mut map = HashMap::new();
                    map.insert(uri.clone(), vec![TextEdit::new(
                        range,
                        s.to_owned(),
                    )]);
                    CodeActionOrCommand::CodeAction(CodeAction {
                        title: "Convert string to double-quotes form"
                            .to_string(),
                        kind: Some(CodeActionKind::REFACTOR),
                        edit: Some(WorkspaceEdit::new(map)),
                        ..Default::default()
                    })
                }
            };
            Some(action)
        });
        Ok(Some(res.collect()))
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "initialized!").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "file opened!").await;
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        let version = params.text_document.version;

        let doc = ropey::Rope::from_str(&text);
        self.doc_map.insert(uri.clone(), doc.clone());

        let mut globals = HashSet::new();
        for global in &self.config.read().unwrap().fennel.diagnostics.globals {
            globals.insert(global.clone());
        }

        let ast = fennel_parser::parse(text.chars(), globals);
        self.publish_diagnostics(&doc, uri.clone(), &ast, Some(version), true)
            .await;

        self.on_save_or_open_errors
            .insert(uri.clone(), ast.on_save_errors().cloned().collect());

        self.ast_map.insert(uri, ast);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;
        let mut doc = if let Some(doc) = self.doc_map.get_mut(&uri) {
            doc
        } else {
            return;
        };

        params.content_changes.iter().for_each(|change| {
            if let Some(lsp_range) = change.range {
                let range = rope_range(&doc, lsp_range).unwrap();
                doc.remove(range.clone());
                if !change.text.is_empty() {
                    doc.insert(range.start, &change.text);
                }
            } else {
                *doc = Rope::from_str(&change.text);
            }
        });

        let mut globals = HashSet::new();
        for global in &self.config.read().unwrap().fennel.diagnostics.globals {
            globals.insert(global.clone());
        }

        let ast = fennel_parser::parse(doc.chars(), globals);
        self.publish_diagnostics(
            &doc,
            uri.clone(),
            &ast,
            Some(version),
            false,
        )
        .await;

        self.ast_map.insert(uri, ast);
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        let ast = self.ast_map.get(&uri).ok_or_else(Error::invalid_request);
        if ast.is_err() {
            return;
        }
        let doc = self.doc_map.get(&uri).ok_or_else(Error::invalid_request);
        if doc.is_err() {
            return;
        }
        self.publish_diagnostics(
            &doc.unwrap(),
            uri,
            &ast.unwrap(),
            None,
            true,
        )
        .await;
    }

    async fn did_change_workspace_folders(
        &self,
        params: DidChangeWorkspaceFoldersParams,
    ) {
        params.event.added.iter().for_each(|r| {
            self.workspace_map.insert(r.uri.clone(), r.name.clone());
        });
        params.event.removed.iter().for_each(|r| {
            self.workspace_map.remove(&r.uri);
        });
    }

    async fn did_change_configuration(
        &self,
        params: DidChangeConfigurationParams,
    ) {
        match <config::Configuration as serde::Deserialize>::deserialize(
            params.settings,
        ) {
            Ok(config) => {
                *self.config.write().unwrap() = config.clone();
                for mut r in self.ast_map.iter_mut() {
                    let ast = r.value_mut();
                    ast.update_globals(
                        config.fennel.diagnostics.globals.clone(),
                    );
                    let uri = r.key();
                    let doc = self.doc_map.get(uri).unwrap();
                    self.publish_diagnostics(
                        &doc,
                        uri.clone(),
                        r.value(),
                        None,
                        false,
                    )
                    .await;
                }
            }
            Err(e) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("Invalid config: {}", e),
                    )
                    .await;
            }
        }
    }
}

impl Backend {
    async fn publish_diagnostics(
        &self,
        doc: &Rope,
        uri: Url,
        ast: &Ast,
        version: Option<i32>,
        on_save_or_open: bool,
    ) {
        if on_save_or_open {
            self.on_save_or_open_errors
                .insert(uri.clone(), ast.on_save_errors().cloned().collect());
        } else if let Some(mut errs) =
            self.on_save_or_open_errors.get_mut(&uri)
        {
            let new_errors: Vec<&fennel_parser::Error> =
                ast.on_save_errors().collect();
            errs.retain(|e| new_errors.contains(&e))
        };

        let errors: Vec<fennel_parser::Error> = if let Some(on_save_errors) =
            self.on_save_or_open_errors.get(&uri)
        {
            ast.errors().chain(on_save_errors.iter()).cloned().collect()
        } else {
            ast.errors().cloned().collect()
        };

        let diagnostics = errors.into_iter().flat_map(|error| {
            lsp_range(doc, error.range).map(|range| {
                let (message, severity) = view::error(error.kind);
                Diagnostic::new(
                    range,
                    Some(severity),
                    None,
                    None,
                    message,
                    None,
                    None,
                )
            })
        });
        self.client
            .publish_diagnostics(uri, diagnostics.collect(), version)
            .await;
    }

    fn find_file(&self, rel: &Url, path: PathBuf) -> Option<Url> {
        path.to_str()?;

        let check_exist = |rel: &Url, ext: &str, init: bool| -> Option<Url> {
            let path = if init { path.join("init") } else { path.clone() };
            if let Ok(url) =
                rel.join(path.with_extension(ext).to_str().unwrap())
            {
                if std::fs::metadata(url.path())
                    .map(|m| m.is_file())
                    .unwrap_or(false)
                {
                    return Some(url);
                }
            }
            None
        };

        let library = &self.config.read().unwrap().fennel.workspace.library;
        let library_file = library.iter().find_map(|uri| {
            let uri_fnl = uri.0.join("fnl/").unwrap();
            let uri_lua = uri.0.join("lua/").unwrap();
            check_exist(&uri_lua, "lua", false)
                .or_else(|| check_exist(&uri_lua, "lua", true))
                .or_else(|| check_exist(&uri_fnl, "fnl", false))
                .or_else(|| check_exist(&uri_fnl, "fnl", true))
        });

        let workspace_file = self.workspace_map.iter().find_map(|ref r| {
            let uri = r.key();
            let uri_fnl = uri.join("fnl/").unwrap();
            check_exist(&uri_fnl, "fnl", false)
                .or_else(|| check_exist(&uri_fnl, "fnl", true))
        });

        workspace_file.or(library_file).or_else(|| {
            check_exist(rel, "lua", false)
                .or_else(|| check_exist(rel, "lua", true))
                .or_else(|| check_exist(rel, "so", false))
                .or_else(|| check_exist(rel, "fnl", false))
                .or_else(|| check_exist(rel, "fnl", true))
        })
    }
}

fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = tower_lsp::LspService::build(|client| Backend {
        client,
        doc_map: DashMap::new(),
        ast_map: DashMap::new(),
        workspace_map: DashMap::new(),
        on_save_or_open_errors: DashMap::new(),
        config: Arc::new(RwLock::new(config::Configuration::default())),
    })
    .finish();
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .unwrap()
        .block_on(async {
            tower_lsp::Server::new(stdin, stdout, socket).serve(service).await;
        })
}
