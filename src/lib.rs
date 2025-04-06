use std::borrow::Cow;

pub use parsers::{
    beatmap::{InvalidEvent, InvalidEventCommand, InvalidHitObject, InvalidTimingPoint},
    CharEnumParseError, EnumParseError, IntEnumParseError, InvalidColour, InvalidRecordField,
    NoBoolValue, RecordParseError,
};
use thiserror::Error;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
    pub fn into_range(self) -> std::ops::Range<usize> {
        self.start..self.end
    }
}

pub mod beatmap;
pub mod skin;

mod parsers;
mod util;

#[derive(Debug, Error)]
pub enum ParseErrorReason {
    #[error("{0}")]
    Bool(
        #[from]
        #[source]
        NoBoolValue,
    ),
    #[error("{0}")]
    InvalidColour(
        #[from]
        #[source]
        InvalidColour,
    ),
    #[error("{0}")]
    InvalidTimingPoint(
        #[from]
        #[source]
        InvalidTimingPoint,
    ),
    #[error("{0}")]
    InvalidHitObject(
        #[from]
        #[source]
        InvalidHitObject,
    ),
    #[error("{0}")]
    InvalidEvent(
        #[from]
        #[source]
        InvalidEvent,
    ),
    #[error("{0}")]
    InvalidEventCommand(
        #[from]
        #[source]
        InvalidEventCommand,
    ),
    #[error("{0}")]
    Int(
        #[from]
        #[source]
        std::num::ParseIntError,
    ),
    #[error("{0}")]
    Float(
        #[from]
        #[source]
        std::num::ParseFloatError,
    ),
    #[error("{0}")]
    Enum(
        #[from]
        #[source]
        EnumParseError,
    ),
    #[error("{0}")]
    Record(
        #[from]
        #[source]
        RecordParseError,
    ),
    #[error("{0}")]
    InvalidRecordField(
        #[from]
        #[source]
        InvalidRecordField,
    ),
    #[error("{0}")]
    IntEnum(
        #[from]
        #[source]
        IntEnumParseError,
    ),
    #[error("{0}")]
    CharEnum(
        #[from]
        #[source]
        CharEnumParseError,
    ),
}

impl From<std::convert::Infallible> for ParseErrorReason {
    fn from(value: std::convert::Infallible) -> Self {
        match value {}
    }
}

#[derive(Debug, Error)]
pub struct ParseError<'a> {
    pub field: Cow<'a, str>,
    pub span: Span,
    #[source]
    pub reason: ParseErrorReason,
}

impl<'a> ParseError<'a> {
    pub(crate) fn curry<E: Into<ParseErrorReason>>(
        field: impl Into<Cow<'a, str>>,
        span: Span,
    ) -> impl FnOnce(E) -> Self {
        move |reason| Self {
            field: field.into(),
            span,
            reason: reason.into(),
        }
    }
}

impl<'a> std::fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("failed to parse ")?;
        f.write_str(&self.field)?;
        f.write_str(": ")?;
        self.reason.fmt(f)
    }
}
