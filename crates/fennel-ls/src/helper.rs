use fennel_parser::TextRange;
use ropey::Rope;
use tower_lsp::{
    jsonrpc::{Error, Result},
    lsp_types::{Position, Range},
};

pub(crate) fn lsp_range(rope: &Rope, range: TextRange) -> Result<Range> {
    let (range_start, range_end) = (range.start(), range.end());
    let pos_start = offset_to_position(rope, range_start.into())?;
    let pos_end = offset_to_position(rope, range_end.into())?;
    Ok(Range::new(pos_start, pos_end))
}

pub(crate) fn position_to_offset(
    rope: &Rope,
    position: Position,
) -> Result<u32> {
    let start_char = rope
        .try_line_to_char(position.line as usize)
        .map_err(|_| Error::invalid_request())?;
    Ok(rope.char_to_byte(start_char + position.character as usize) as u32)
}

pub(crate) fn offset_to_position(
    rope: &Rope,
    offset: usize,
) -> Result<Position> {
    let line =
        rope.try_byte_to_line(offset).map_err(|_| Error::invalid_request())?;
    let start_char = rope.line_to_char(line);
    let column = rope.byte_to_char(offset) - start_char;
    Ok(Position::new(line as u32, column as u32))
}
