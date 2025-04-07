use std::borrow::Cow;

pub use parsers::{
    beatmap::{InvalidEvent, InvalidEventCommand, InvalidHitObject, InvalidTimingPoint},
    skin::MissingKeycountError,
    CharEnumParseError, EnumParseError, IntEnumParseError, InvalidColour, InvalidRecordField,
    NoBoolValue, RecordParseError,
};
use thiserror::Error;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: u64,
    pub end: u64,
}

impl Span {
    pub fn new(start: u64, end: u64) -> Self {
        Self { start, end }
    }
    pub fn into_range(self) -> std::ops::Range<u64> {
        self.start..self.end
    }
}

pub mod beatmap;
pub mod skin;

mod parsers;
mod util;

#[derive(Debug, Error)]
pub struct UnknownSectionName;

impl std::fmt::Display for UnknownSectionName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("unknown section name")
    }
}

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
    #[error("{0}")]
    MissingKeycount(
        #[from]
        #[source]
        MissingKeycountError,
    ),
    #[error("{0}")]
    UnknownSectionName(
        #[from]
        #[source]
        UnknownSectionName,
    ),
}

impl From<std::convert::Infallible> for ParseErrorReason {
    fn from(value: std::convert::Infallible) -> Self {
        match value {}
    }
}

#[derive(Debug, Error)]
pub struct ParseError {
    pub field: Cow<'static, str>,
    pub span: Span,
    #[source]
    pub reason: ParseErrorReason,
}

impl ParseError {
    pub(crate) fn curry<E: Into<ParseErrorReason>>(
        field: impl Into<Cow<'static, str>>,
        span: Span,
    ) -> impl FnOnce(E) -> Self {
        move |reason| Self {
            field: field.into(),
            span,
            reason: reason.into(),
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("failed to parse ")?;
        f.write_str(&self.field)?;
        f.write_str(": ")?;
        self.reason.fmt(f)
    }
}
