mod config;
mod helper;
mod view;

use std::{
    collections::{HashMap, HashSet},
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
}

#[tower_lsp::async_trait]
impl tower_lsp::LanguageServer for Backend {
    async fn initialize(
        &self,
        _: InitializeParams,
    ) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: env!("CARGO_PKG_NAME").into(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".into(), ":".into()]),
                    ..Default::default()
                }),
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
        let offset = position_to_offset(&doc, position)?;

        if let Some((symbol, _)) = ast.definition(offset) {
            let range = lsp_range(&doc, symbol.token.range)?;
            Ok(Some(GotoDefinitionResponse::Scalar(Location::new(uri, range))))
        } else {
            Ok(None)
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
        let offset = position_to_offset(&doc, position)?;

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
        let offset = position_to_offset(&doc, position)?;

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
        let offset = position_to_offset(&doc, position)?;

        let definition = ast.definition(offset);
        if definition.is_none() {
            return Ok(None);
        }
        let (symbol, _) = definition.unwrap();
        let range = lsp_range(&doc, symbol.token.range)?;
        let text = symbol.token.text;
        let scope_kind = view::scope_kind(symbol.scope.kind);
        let value_kind = view::value_kind(symbol.value.kind);

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
        let offset = position_to_offset(&doc, position)?;

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

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "initialized!").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "file opened!").await;
        self.on_change(
            params.text_document.uri,
            params.text_document.text,
            params.text_document.version,
        )
        .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(
            params.text_document.uri,
            std::mem::take(&mut params.content_changes[0].text),
            params.text_document.version,
        )
        .await
    }

    async fn did_change_configuration(
        &self,
        params: DidChangeConfigurationParams,
    ) {
        if let Ok(config) =
            <config::Configuration as serde::Deserialize>::deserialize(
                params.settings,
            )
        {
            *self.config.write().unwrap() = config.clone();
            for mut r in self.ast_map.iter_mut() {
                let ast = r.value_mut();
                ast.update_globals(config.fennel.diagnostics.globals.clone());
                let uri = r.key();
                let doc = self.doc_map.get(uri).unwrap();
                self.publish_diagnostics(&doc, uri.clone(), r.value(), None)
                    .await;
            }
        }
    }
}

impl Backend {
    async fn on_change(&self, uri: Url, text: String, version: i32) {
        let doc = ropey::Rope::from_str(&text);
        self.doc_map.insert(uri.clone(), doc.clone());

        let mut globals = HashSet::new();
        for global in &self.config.read().unwrap().fennel.diagnostics.globals {
            globals.insert(global.clone());
        }

        let ast = fennel_parser::parse(&text, globals);
        self.publish_diagnostics(&doc, uri.clone(), &ast, Some(version)).await;
        self.ast_map.insert(uri, ast);
    }

    async fn publish_diagnostics(
        &self,
        doc: &Rope,
        uri: Url,
        ast: &Ast,
        version: Option<i32>,
    ) {
        let diagnostics = ast.errors().flat_map(|error| {
            lsp_range(doc, error.range).map(|range| {
                let message = view::error(error.kind);
                Diagnostic::new_simple(range, message)
            })
        });
        self.client
            .publish_diagnostics(uri, diagnostics.collect(), version)
            .await;
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = tower_lsp::LspService::build(|client| Backend {
        client,
        doc_map: DashMap::new(),
        ast_map: DashMap::new(),
        config: Arc::new(RwLock::new(config::Configuration::default())),
    })
    .finish();
    tower_lsp::Server::new(stdin, stdout, socket).serve(service).await;
}
