use std::borrow::Cow;

use thiserror::Error;

use crate::{util::StaticCow, ParseError};

use super::InvalidColour;

pub trait ParseField<'a>: Sized {
    fn parse_field(
        name: impl Into<Cow<'static, str>>,
        text: impl StaticCow<'a>,
    ) -> Result<Self, ParseError>;
}

pub trait ParseFieldInPlace<'a>: Sized {
    fn parse_field_in_place(
        &mut self,
        name: impl Into<Cow<'static, str>>,
        text: impl StaticCow<'a>,
    ) -> Result<(), ParseError>;
}

impl<'a, T: ParseField<'a>> ParseFieldInPlace<'a> for T {
    fn parse_field_in_place(
        &mut self,
        name: impl Into<Cow<'static, str>>,
        text: impl StaticCow<'a>,
    ) -> Result<(), ParseError> {
        let ret = T::parse_field(name, text)?;
        *self = ret;
        Ok(())
    }
}

macro_rules! impl_parse_field {
    ($($T:ty),*) => {
        $(impl<'a> ParseField<'a> for $T {
            fn parse_field(
                name: impl Into<Cow<'static, str>>,
                line: impl StaticCow<'a>,
            ) -> Result<Self, ParseError> {
                line.as_ref().parse().map_err(ParseError::curry(name, line.span()))
            }
        })*
    };
}

impl_parse_field!(u8, u16, u32, u64, usize);
impl_parse_field!(i8, i16, i32, i64, isize);
impl_parse_field!(f32, f64);

impl<'a, T: ParseField<'a>> ParseField<'a> for Option<T> {
    fn parse_field(
        name: impl Into<Cow<'static, str>>,
        text: impl StaticCow<'a>,
    ) -> Result<Self, ParseError> {
        if text.as_ref().is_empty() {
            Ok(None)
        } else {
            T::parse_field(name, text).map(Some)
        }
    }
}

impl<'a> ParseField<'a> for bool {
    fn parse_field(
        name: impl Into<Cow<'static, str>>,
        text: impl StaticCow<'a>,
    ) -> Result<Self, ParseError> {
        match text.as_ref().as_bytes() {
            [b't' | b'T', b'r' | b'R', b'u' | b'U', b'e' | b'E'] => Ok(true),
            [b'f' | b'F', b'a' | b'A', b'l' | b'L', b's' | b'S', b'e' | b'E'] => Ok(false),
            _ => Ok(i32::parse_field(name, text)? != 0),
        }
    }
}

impl<'a> ParseField<'a> for (u8, u8, u8, u8) {
    fn parse_field(
        name: impl Into<Cow<'static, str>>,
        text: impl StaticCow<'a>,
    ) -> Result<Self, ParseError> {
        let span = text.span();
        let mut c = text.split(',');
        let r = match c.next() {
            Some(x) => x,
            None => return Err(ParseError::curry(name, span)(InvalidColour)),
        };
        let g = match c.next() {
            Some(x) => x,
            None => return Err(ParseError::curry(name, span)(InvalidColour)),
        };
        let b = match c.next() {
            Some(x) => x,
            None => return Err(ParseError::curry(name, span)(InvalidColour)),
        };
        let a = c.next();
        if a.is_some() && c.next().is_some() {
            return Err(ParseError::curry(name, span)(InvalidColour));
        }
        let r = u8::parse_field("red colour channel", r)?;
        let g = u8::parse_field("green colour channel", g)?;
        let b = u8::parse_field("blue colour channel", b)?;
        let a = a
            .map(|a| u8::parse_field("alpha colour channel", a))
            .transpose()?;
        Ok((r, g, b, a.unwrap_or(255)))
    }
}

impl<'a> ParseField<'a> for (u8, u8, u8) {
    fn parse_field(
        name: impl Into<Cow<'static, str>>,
        text: impl StaticCow<'a>,
    ) -> Result<Self, ParseError> {
        let (r, g, b, _a) = <(u8, u8, u8, u8)>::parse_field(name, text)?;
        Ok((r, g, b))
    }
}

impl<'a, T: ParseField<'a>> ParseFieldInPlace<'a> for Vec<T> {
    fn parse_field_in_place(
        &mut self,
        _name: impl Into<Cow<'static, str>>,
        text: impl StaticCow<'a>,
    ) -> Result<(), ParseError> {
        self.clear();
        let elems = text.split(',').map(|x| T::parse_field("list element", x));
        self.reserve(elems.size_hint().0);
        for elem in elems {
            self.push(elem?);
        }
        Ok(())
    }
}

impl<'a, T: ParseField<'a>> ParseFieldInPlace<'a> for Box<[T]> {
    fn parse_field_in_place(
        &mut self,
        _name: impl Into<Cow<'static, str>>,
        text: impl StaticCow<'a>,
    ) -> Result<(), ParseError> {
        for (out, elem) in self
            .iter_mut()
            .zip(text.split(',').map(|x| T::parse_field("list element", x)))
        {
            *out = elem?;
        }
        Ok(())
    }
}

impl<'a> ParseField<'a> for Cow<'a, str> {
    fn parse_field(
        _name: impl Into<Cow<'static, str>>,
        text: impl StaticCow<'a>,
    ) -> Result<Self, ParseError> {
        Ok(text.into_cow())
    }
}

#[derive(Debug, Error)]
pub struct MissingKeycountError;

impl std::fmt::Display for MissingKeycountError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("mania section has missing keycount")
    }
}
