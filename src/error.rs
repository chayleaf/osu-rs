use std::borrow::Cow;

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

#[derive(Debug, Error)]
pub struct UnknownSectionName;

impl std::fmt::Display for UnknownSectionName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("unknown section name")
    }
}

#[derive(Debug, Error)]
pub struct MissingKeycountError;

impl std::fmt::Display for MissingKeycountError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("mania section has missing keycount")
    }
}

#[derive(Debug, Error)]
pub struct InvalidTimingPoint;

impl std::fmt::Display for InvalidTimingPoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("invalid timing point")
    }
}

#[derive(Debug, Error)]
pub struct InvalidHitObject;

impl std::fmt::Display for InvalidHitObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("invalid hit object")
    }
}

#[derive(Debug, Error)]
pub struct InvalidEvent;

impl std::fmt::Display for InvalidEvent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("invalid event")
    }
}

#[derive(Debug, Error)]
pub struct InvalidEventCommand;

impl std::fmt::Display for InvalidEventCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("invalid event command")
    }
}

#[derive(Debug, Error)]
pub struct NoBoolValue;

impl std::fmt::Display for NoBoolValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("missing boolean value")
    }
}

#[derive(Debug, Error)]
pub struct InvalidColour;

impl std::fmt::Display for InvalidColour {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("invalid colour")
    }
}

#[derive(Debug, Error)]
pub struct EnumParseError {
    pub valid_variants: &'static [&'static str],
}

impl std::fmt::Display for EnumParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("unexpected enum value (valid values: ")?;
        let mut first = true;
        for x in self.valid_variants {
            if first {
                first = false;
            } else {
                f.write_str(", ")?;
            }
            f.write_str(x)?;
        }
        f.write_str(")")
    }
}

#[derive(Debug, Error)]
pub struct IntEnumParseError {
    pub variant: i32,
    pub valid_variants: &'static [i32],
}

impl std::fmt::Display for IntEnumParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "unexpected enum value {} (valid values: ", self.variant)?;
        let mut first = true;
        for x in self.valid_variants {
            if first {
                first = false;
            } else {
                f.write_str(", ")?;
            }
            x.fmt(f)?;
        }
        f.write_str(")")
    }
}

#[derive(Debug, Error)]
pub struct CharEnumParseError {
    pub variant: char,
    pub valid_variants: &'static [char],
}

impl std::fmt::Display for CharEnumParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "unexpected enum value {} (valid values: ", self.variant)?;
        let mut first = true;
        for x in self.valid_variants {
            if first {
                first = false;
            } else {
                f.write_str(", ")?;
            }
            x.fmt(f)?;
        }
        f.write_str(")")
    }
}

#[derive(Debug, Error)]
pub struct InvalidRecordField;

impl std::fmt::Display for InvalidRecordField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "invalid record field")
    }
}

#[derive(Debug, Error)]
pub struct RecordParseError {
    pub valid_fields: &'static [&'static str],
}

impl std::fmt::Display for RecordParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("unexpected field (valid fields: ")?;
        let mut first = true;
        for x in self.valid_fields {
            if first {
                first = false;
            } else {
                f.write_str(", ")?;
            }
            f.write_str(x)?;
        }
        f.write_str(")")
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
