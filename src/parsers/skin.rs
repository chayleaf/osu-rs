use std::borrow::Cow;

use crate::{util::StaticCow, ParseError};

use super::InvalidColour;

pub trait ParseField<'a>: Sized {
    fn parse_field(name: &'a str, text: impl StaticCow<'a>) -> Result<Self, ParseError<'a>>;
}

macro_rules! impl_parse_field {
    ($($T:ty),*) => {
        $(impl<'a> ParseField<'a> for $T {
            fn parse_field(
                name: &'a str,
                line: impl StaticCow<'a>,
            ) -> Result<Self, ParseError<'a>> {
                line.as_ref().parse().map_err(ParseError::curry(name, line.span()))
            }
        })*
    };
}

impl_parse_field!(u8, u16, u32, u64, usize);
impl_parse_field!(i8, i16, i32, i64, isize);
impl_parse_field!(f32, f64);

impl<'a, T: ParseField<'a>> ParseField<'a> for Option<T> {
    fn parse_field(name: &'a str, text: impl StaticCow<'a>) -> Result<Self, ParseError<'a>> {
        if text.as_ref().is_empty() {
            Ok(None)
        } else {
            T::parse_field(name, text).map(Some)
        }
    }
}

impl<'a> ParseField<'a> for bool {
    fn parse_field(name: &'a str, text: impl StaticCow<'a>) -> Result<Self, ParseError<'a>> {
        match text.as_ref().as_bytes() {
            [b't' | b'T', b'r' | b'R', b'u' | b'U', b'e' | b'E'] => Ok(true),
            [b'f' | b'F', b'a' | b'A', b'l' | b'L', b's' | b'S', b'e' | b'E'] => Ok(false),
            _ => Ok(i32::parse_field(name, text)? != 0),
        }
    }
}

impl<'a> ParseField<'a> for (u8, u8, u8, u8) {
    fn parse_field(name: &'a str, text: impl StaticCow<'a>) -> Result<Self, ParseError<'a>> {
        let span = text.span();
        let mut c = text.split(',');
        let r = c
            .next()
            .ok_or(InvalidColour)
            .map_err(ParseError::curry(name, span))?;
        let g = c
            .next()
            .ok_or(InvalidColour)
            .map_err(ParseError::curry(name, span))?;
        let b = c
            .next()
            .ok_or(InvalidColour)
            .map_err(ParseError::curry(name, span))?;
        let a = c.next();
        if a.is_some() && c.next().is_some() {
            return Err(ParseError::curry(name, span)(InvalidColour));
        }
        let r = u8::parse_field(name, r)?;
        let g = u8::parse_field(name, g)?;
        let b = u8::parse_field(name, b)?;
        let a = a.map(|a| u8::parse_field(name, a)).transpose()?;
        Ok((r, g, b, a.unwrap_or(255)))
    }
}

impl<'a> ParseField<'a> for (u8, u8, u8) {
    fn parse_field(name: &'a str, text: impl StaticCow<'a>) -> Result<Self, ParseError<'a>> {
        let (r, g, b, _a) = <(u8, u8, u8, u8)>::parse_field(name, text)?;
        Ok((r, g, b))
    }
}

impl<'a, T: ParseField<'a>> ParseField<'a> for Vec<T> {
    fn parse_field(name: &'a str, text: impl StaticCow<'a>) -> Result<Self, ParseError<'a>> {
        text.split(',').map(|x| T::parse_field(name, x)).collect()
    }
}

impl<'a> ParseField<'a> for Cow<'a, str> {
    fn parse_field(_name: &'a str, text: impl StaticCow<'a>) -> Result<Self, ParseError<'a>> {
        Ok(text.into_cow())
    }
}
