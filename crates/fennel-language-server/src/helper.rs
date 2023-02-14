use fennel_parser::TextRange;
use ropey::Rope;
use tower_lsp::{
    jsonrpc::{Error, Result},
    lsp_types::{Position, Range},
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
    let utf16_cu =
        rope.char_to_utf16_cu(start_char) + position.character as usize;
    Ok(rope.utf16_cu_to_char(utf16_cu))
}

pub(crate) fn position_to_byte_offset(
    rope: &Rope,
    position: Position,
) -> Result<u32> {
    let start_char = rope
        .try_line_to_char(position.line as usize)
        .map_err(|_| Error::invalid_request())?;
    let utf16_cu =
        rope.char_to_utf16_cu(start_char) + position.character as usize;
    let char = rope.utf16_cu_to_char(utf16_cu);
    Ok(rope.char_to_byte(char) as u32)
}

pub(crate) fn byte_offset_to_position(
    rope: &Rope,
    offset: usize,
) -> Result<Position> {
    let line =
        rope.try_byte_to_line(offset).map_err(|_| Error::invalid_request())?;
    let start_char = rope.line_to_char(line);
    let utf16_cu = rope.char_to_utf16_cu(start_char);
    let column = rope.char_to_utf16_cu(rope.byte_to_char(offset)) - utf16_cu;
    Ok(Position::new(line as u32, column as u32))
}

pub(crate) fn lsp_range_head() -> Range {
    Range::new(Position::new(0, 0), Position::new(0, 0))
}
