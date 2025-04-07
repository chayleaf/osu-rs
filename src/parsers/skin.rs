use std::{borrow::Cow, io};

use crate::{
    error::{InvalidColour, ParseError}, skin::{ManiaNoteBodyStyle, ManiaSpecialStyle, SliderStyle}, util::StaticCow
};

pub trait SerializeField {
    fn should_serialize(&self) -> bool {
        true
    }
    fn serialize(&self, out: &mut impl io::Write) -> io::Result<()> {
        self.serialize_compact(out)
    }
    fn serialize_compact_in_list(&self, out: &mut impl io::Write) -> io::Result<()> {
        self.serialize_compact(out)
    }
    fn serialize_compact(&self, out: &mut impl io::Write) -> io::Result<()>;
}
pub trait SerializeBox<T> {
    fn should_serialize_box(&self) -> bool {
        true
    }
    fn serialize_box(&self, out: &mut impl io::Write) -> io::Result<()>;
    fn serialize_compact_box(&self, out: &mut impl io::Write, default: T) -> io::Result<()>;
}

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

macro_rules! impl_field {
    ($($T:ty),*) => {
        $(impl<'a> ParseField<'a> for $T {
            fn parse_field(
                name: impl Into<Cow<'static, str>>,
                line: impl StaticCow<'a>,
            ) -> Result<Self, ParseError> {
                line.as_ref().parse().map_err(ParseError::curry(name, line.span()))
            }
        }
        impl SerializeField for $T {
            fn serialize_compact(&self, out: &mut impl io::Write) -> io::Result<()> {
                write!(out, "{self}")
            }
            fn serialize_compact_in_list(&self, out: &mut impl io::Write) -> io::Result<()> {
                if *self != Self::default() {
                    write!(out, "{self}")
                } else {
                    Ok(())
                }
            }
        })*
    };
}

impl_field!(u8, u16, u32, u64, usize);
impl_field!(i8, i16, i32, i64, isize);
impl_field!(f32, f64);

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
impl<T: SerializeField> SerializeField for Option<T> {
    fn should_serialize(&self) -> bool {
        self.is_some()
    }
    fn serialize_compact(&self, out: &mut impl io::Write) -> io::Result<()> {
        if let Some(val) = self {
            val.serialize_compact(out)
        } else {
            Ok(())
        }
    }
    fn serialize(&self, out: &mut impl io::Write) -> io::Result<()> {
        if let Some(val) = self {
            val.serialize(out)
        } else {
            Ok(())
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

impl SerializeField for bool {
    fn serialize(&self, out: &mut impl io::Write) -> io::Result<()> {
        if *self {
            write!(out, "true")
        } else {
            write!(out, "false")
        }
    }
    fn serialize_compact(&self, out: &mut impl io::Write) -> io::Result<()> {
        if *self {
            write!(out, "1")
        } else {
            write!(out, "0")
        }
    }
    fn serialize_compact_in_list(&self, out: &mut impl io::Write) -> io::Result<()> {
        if *self {
            write!(out, "1")
        } else {
            Ok(())
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

impl SerializeField for (u8, u8, u8, u8) {
    fn serialize(&self, out: &mut impl io::Write) -> io::Result<()> {
        (self.0, self.1, self.2).serialize_compact(out)?;
        write!(out, ",{}", self.3)
    }
    fn serialize_compact(&self, out: &mut impl io::Write) -> io::Result<()> {
        (self.0, self.1, self.2).serialize_compact(out)?;
        if self.3 == 255 {
            Ok(())
        } else {
            write!(out, ",{}", self.3)
        }
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

impl SerializeField for (u8, u8, u8) {
    fn serialize_compact(&self, out: &mut impl io::Write) -> io::Result<()> {
        write!(out, "{},{},{}", self.0, self.1, self.2)
    }
}

impl<'a, T: Default + ParseField<'a>> ParseFieldInPlace<'a> for Vec<T> {
    fn parse_field_in_place(
        &mut self,
        _name: impl Into<Cow<'static, str>>,
        text: impl StaticCow<'a>,
    ) -> Result<(), ParseError> {
        self.clear();
        let elems = text.split(',').map(|x| T::parse_field("list element", x));
        self.reserve(elems.size_hint().0);
        for elem in elems {
            self.push(elem.unwrap_or_default());
        }
        Ok(())
    }
}

impl<T: SerializeField> SerializeField for Vec<T> {
    fn should_serialize(&self) -> bool {
        !self.is_empty()
    }
    fn serialize(&self, out: &mut impl io::Write) -> io::Result<()> {
        let mut first = true;
        for elem in self {
            if first {
                first = false;
            } else {
                write!(out, ",")?;
            }
            elem.serialize(out)?;
        }
        Ok(())
    }
    fn serialize_compact(&self, out: &mut impl io::Write) -> io::Result<()> {
        let mut first = true;
        for elem in self {
            if first {
                first = false;
            } else {
                write!(out, ",")?;
            }
            if self.len() == 1 {
                elem.serialize_compact(out)?;
            } else {
                elem.serialize_compact_in_list(out)?;
            }
        }
        Ok(())
    }
}

impl<'a, T: Default + ParseField<'a>> ParseFieldInPlace<'a> for Box<[T]> {
    fn parse_field_in_place(
        &mut self,
        _name: impl Into<Cow<'static, str>>,
        text: impl StaticCow<'a>,
    ) -> Result<(), ParseError> {
        for (out, elem) in self
            .iter_mut()
            .zip(text.split(',').map(|x| T::parse_field("list element", x)))
        {
            *out = elem.unwrap_or_default();
        }
        Ok(())
    }
}
impl<T: SerializeField + PartialEq> SerializeBox<T> for Box<[T]> {
    fn should_serialize_box(&self) -> bool {
        !self.is_empty()
    }
    fn serialize_box(&self, out: &mut impl io::Write) -> io::Result<()> {
        let mut first = true;
        for elem in self {
            if first {
                first = false;
            } else {
                write!(out, ",")?;
            }
            elem.serialize(out)?;
        }
        Ok(())
    }
    fn serialize_compact_box(&self, out: &mut impl io::Write, default: T) -> io::Result<()> {
        // unwrap: ok because this method is only called when any element doesn't equal default, so
        //         we must find at least one
        let count = self
            .iter()
            .enumerate()
            .rev()
            .find(|(_i, x)| **x != default)
            .unwrap()
            .0
            + 1;
        let mut first = true;
        for elem in self.iter().take(count) {
            if first {
                first = false;
            } else {
                write!(out, ",")?;
            }
            if self.len() == 1 {
                elem.serialize_compact(out)?;
            } else {
                elem.serialize_compact_in_list(out)?;
            }
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

impl SerializeField for SliderStyle {
    fn serialize_compact(&self, out: &mut impl io::Write) -> io::Result<()> {
        write!(out, "{}", *self as i32)
    }
}

impl<'a> SerializeField for Cow<'a, str> {
    fn serialize_compact(&self, out: &mut impl io::Write) -> io::Result<()> {
        write!(out, "{self}")
    }
}

impl SerializeField for ManiaSpecialStyle {
    fn serialize_compact(&self, out: &mut impl io::Write) -> io::Result<()> {
        write!(out, "{}", *self as i32)
    }
}

impl SerializeField for ManiaNoteBodyStyle {
    fn serialize_compact(&self, out: &mut impl io::Write) -> io::Result<()> {
        write!(out, "{}", *self as i32)
    }
}
