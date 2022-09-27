use std::path::PathBuf;

use fennel_parser::TextRange;
use ropey::Rope;
use tower_lsp::{
    jsonrpc::{Error, Result},
    lsp_types::{Position, Range, Url},
};

pub(crate) fn lsp_range(rope: &Rope, range: TextRange) -> Result<Range> {
    let (range_start, range_end) = (range.start(), range.end());
    let pos_start = byte_offset_to_position(rope, range_start.into())?;
    let pos_end = byte_offset_to_position(rope, range_end.into())?;
    Ok(Range::new(pos_start, pos_end))
}

pub(crate) fn rope_range(
    rope: &Rope,
    lsp_range: Range,
) -> Result<std::ops::Range<usize>> {
    let (pos_start, pos_end) = (lsp_range.start, lsp_range.end);
    let range_start = position_to_char_idx(rope, pos_start)?;
    let range_end = position_to_char_idx(rope, pos_end)?;
    Ok(range_start..range_end)
}

pub(crate) fn position_to_char_idx(
    rope: &Rope,
    position: Position,
) -> Result<usize> {
    let start_char = rope
        .try_line_to_char(position.line as usize)
        .map_err(|_| Error::invalid_request())?;
    Ok(start_char + position.character as usize)
}

pub(crate) fn position_to_byte_offset(
    rope: &Rope,
    position: Position,
) -> Result<u32> {
    let start_char = rope
        .try_line_to_char(position.line as usize)
        .map_err(|_| Error::invalid_request())?;
    Ok(rope.char_to_byte(start_char + position.character as usize) as u32)
}

pub(crate) fn byte_offset_to_position(
    rope: &Rope,
    offset: usize,
) -> Result<Position> {
    let line =
        rope.try_byte_to_line(offset).map_err(|_| Error::invalid_request())?;
    let start_char = rope.line_to_char(line);
    let column = rope.byte_to_char(offset) - start_char;
    Ok(Position::new(line as u32, column as u32))
}

pub(crate) fn lsp_range_head() -> Range {
    Range::new(Position::new(0, 0), Position::new(0, 0))
}

pub(crate) fn find_file(rel: &Url, path: PathBuf) -> Option<Url> {
    path.to_str()?;

    let check_exist = |ext: &str, init: bool| -> Option<Url> {
        let path = if init { path.join("init") } else { path.clone() };
        if let Ok(url) = rel.join(path.with_extension(ext).to_str().unwrap()) {
            if std::fs::metadata(url.path())
                .map(|m| m.is_file())
                .unwrap_or(false)
            {
                return Some(url);
            }
        }
        None
    };
    check_exist("lua", false)
        .or_else(|| check_exist("lua", true))
        .or_else(|| check_exist("so", false))
        .or_else(|| check_exist("fnl", false))
        .or_else(|| check_exist("fnl", true))
}
