use std::borrow::{Borrow, Cow};

use crate::Span;

pub trait StaticCow<'a>: Copy + Clone + Sized + Borrow<str> + AsRef<str> {
    fn new(s: &'static str, span: Span) -> Self;
    fn substr(&self, i: usize) -> Self;
    fn span(&self) -> Span;
    fn into_cow(self) -> Cow<'a, str>;
    fn split(&self, p: char) -> impl Iterator<Item = Self>;
    fn split_once(&self, p: char) -> Option<(Self, Self)>;
    fn split_once2(&self, p: &str) -> Option<(Self, Self)>;
    fn trim(&self) -> Self;
    fn trim_matches(&self, c: char) -> Self;
    fn trim_matches2(&self, c1: char, c2: char) -> Self;
    fn trim_start_matches(&self, c: char) -> Self;
}

#[derive(Copy, Clone)]
pub struct Lended<'a>(pub &'a str, pub Span);
impl<'a> StaticCow<'a> for Lended<'_> {
    fn new(s: &'static str, span: Span) -> Self {
        Self(s, span)
    }
    fn substr(&self, i: usize) -> Self {
        Self(&self.0[i..], Span::new(self.1.start + 1, self.1.end))
    }
    fn span(&self) -> Span {
        self.1
    }
    fn into_cow(self) -> Cow<'a, str> {
        Cow::Owned(Cow::Borrowed(self.0).into_owned())
    }
    fn split(&self, p: char) -> impl Iterator<Item = Self> {
        let mut cur = self.1.start;
        self.0.split(p).map(move |x| {
            let span = Span::new(cur, cur + x.len() as u64);
            cur += 1 + x.len() as u64;
            Self(x, span)
        })
    }
    fn split_once(&self, p: char) -> Option<(Self, Self)> {
        self.0.split_once(p).map(|(a, b)| {
            (
                Self(a, Span::new(self.1.start, self.1.start + a.len() as u64)),
                Self(b, Span::new(self.1.end - b.len() as u64, self.1.end)),
            )
        })
    }
    fn split_once2(&self, p: &str) -> Option<(Self, Self)> {
        self.0.split_once(p).map(|(a, b)| {
            (
                Self(a, Span::new(self.1.start, self.1.start + a.len() as u64)),
                Self(b, Span::new(self.1.end - b.len() as u64, self.1.end)),
            )
        })
    }
    fn trim(&self) -> Self {
        let mut span = self.1;
        let value = self.0.trim_start();
        span.start = span.end - value.len() as u64;
        let value = value.trim_end();
        span.end = span.start + value.len() as u64;
        Self(value, span)
    }
    fn trim_matches(&self, c: char) -> Self {
        let mut span = self.1;
        let value = self.0.trim_start_matches(c);
        span.start = span.end - value.len() as u64;
        let value = value.trim_end_matches(c);
        span.end = span.start + value.len() as u64;
        Self(value, span)
    }
    fn trim_matches2(&self, c1: char, c2: char) -> Self {
        let mut span = self.1;
        let value = self.0.trim_start_matches([c1, c2]);
        span.start = span.end - value.len() as u64;
        let value = value.trim_end_matches([c1, c2]);
        span.end = span.start + value.len() as u64;
        Self(value, span)
    }
    fn trim_start_matches(&self, c: char) -> Self {
        let mut span = self.1;
        let value = self.0.trim_start_matches(c);
        span.start = span.end - value.len() as u64;
        Self(value, span)
    }
}

impl AsRef<str> for Lended<'_> {
    fn as_ref(&self) -> &str {
        self.0
    }
}

impl Borrow<str> for Lended<'_> {
    fn borrow(&self) -> &str {
        self.0
    }
}

#[derive(Copy, Clone)]
pub struct Borrowed<'a>(pub &'a str, pub Span);
impl<'a> StaticCow<'a> for Borrowed<'a> {
    fn new(s: &'static str, span: Span) -> Self {
        Self(s, span)
    }
    fn substr(&self, i: usize) -> Self {
        Self(&self.0[i..], Span::new(self.1.start + 1, self.1.end))
    }
    fn span(&self) -> Span {
        self.1
    }
    fn into_cow(self) -> Cow<'a, str> {
        Cow::Borrowed(self.0)
    }
    fn split(&self, p: char) -> impl Iterator<Item = Self> {
        let mut cur = self.1.start;
        self.0.split(p).map(move |x| {
            let span = Span::new(cur, cur + x.len() as u64);
            cur += 1 + x.len() as u64;
            Self(x, span)
        })
    }
    fn split_once(&self, p: char) -> Option<(Self, Self)> {
        self.0.split_once(p).map(|(a, b)| {
            (
                Self(a, Span::new(self.1.start, self.1.start + a.len() as u64)),
                Self(b, Span::new(self.1.end - b.len() as u64, self.1.end)),
            )
        })
    }
    fn split_once2(&self, p: &str) -> Option<(Self, Self)> {
        self.0.split_once(p).map(|(a, b)| {
            (
                Self(a, Span::new(self.1.start, self.1.start + a.len() as u64)),
                Self(b, Span::new(self.1.end - b.len() as u64, self.1.end)),
            )
        })
    }
    fn trim(&self) -> Self {
        let mut span = self.1;
        let value = self.0.trim_start();
        span.start = span.end - value.len() as u64;
        let value = value.trim_end();
        span.end = span.start + value.len() as u64;
        Self(value, span)
    }
    fn trim_matches(&self, c: char) -> Self {
        let mut span = self.1;
        let value = self.0.trim_start_matches(c);
        span.start = span.end - value.len() as u64;
        let value = value.trim_end_matches(c);
        span.end = span.start + value.len() as u64;
        Self(value, span)
    }
    fn trim_matches2(&self, c1: char, c2: char) -> Self {
        let mut span = self.1;
        let value = self.0.trim_start_matches([c1, c2]);
        span.start = span.end - value.len() as u64;
        let value = value.trim_end_matches([c1, c2]);
        span.end = span.start + value.len() as u64;
        Self(value, span)
    }
    fn trim_start_matches(&self, c: char) -> Self {
        let mut span = self.1;
        let value = self.0.trim_start_matches(c);
        span.start = span.end - value.len() as u64;
        Self(value, span)
    }
}

impl<'a> AsRef<str> for Borrowed<'a> {
    fn as_ref(&self) -> &str {
        self.0
    }
}

impl<'a> Borrow<str> for Borrowed<'a> {
    fn borrow(&self) -> &str {
        self.0
    }
}
