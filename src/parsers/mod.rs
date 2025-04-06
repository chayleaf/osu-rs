use thiserror::Error;

pub(crate) mod beatmap;
pub(crate) mod skin;

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
