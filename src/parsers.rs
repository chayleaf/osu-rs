use std::borrow::Cow;

use thiserror::Error;

use crate::{
    BeatmapSection, Context, FullHitSound, HitObject, HitObjectFlags, HitObjectKind, HitSound,
    SampleSet, SliderKind, SoundTypes, Span, StaticCow, TimingPoint, TimingPointFlags,
};

pub trait ParseField<'a>: Sized {
    fn parse_field(
        name: impl Into<Cow<'static, str>>,
        ctx: &Context,
        line: impl StaticCow<'a>,
    ) -> Result<Self, ParseError>;
}

macro_rules! impl_parse_field {
    ($($T:ty),*) => {
        $(impl<'a> ParseField<'a> for $T {
            fn parse_field(
                name: impl Into<Cow<'static, str>>,
                _ctx: &Context,
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
impl_parse_field!(TimingPointFlags);

impl<'a, T: ParseField<'a>> BeatmapSection<'a> for Vec<T> {
    fn consume_line(
        &mut self,
        ctx: &Context,
        line: impl StaticCow<'a>,
    ) -> Result<Option<crate::Section>, ParseError> {
        self.push(ParseField::parse_field("", ctx, line)?);
        Ok(None)
    }
}

impl<'a> ParseField<'a> for Vec<i32> {
    fn parse_field(
        name: impl Into<Cow<'static, str>>,
        ctx: &Context,
        line: impl StaticCow<'a>,
    ) -> Result<Self, ParseError> {
        let mut ret = vec![];
        let name = name.into();
        for val in line.split(',') {
            if !val.as_ref().is_empty() {
                ret.push(ParseField::parse_field(name.clone(), ctx, val)?);
            }
        }
        Ok(ret)
    }
}

impl<'a> ParseField<'a> for bool {
    fn parse_field(
        name: impl Into<Cow<'static, str>>,
        _ctx: &Context,
        line: impl StaticCow<'a>,
    ) -> Result<Self, ParseError> {
        match line.as_ref().bytes().next() {
            Some(b'1') => Ok(true),
            Some(_) => Ok(false),
            None => Err(ParseError::curry(name, line.span())(NoBoolValue)),
        }
    }
}

impl<'a> ParseField<'a> for TimingPoint {
    fn parse_field(
        _name: impl Into<Cow<'static, str>>,
        ctx: &Context,
        line: impl StaticCow<'a>,
    ) -> Result<Self, ParseError> {
        let mut end_span = line.span();
        end_span.start = end_span.end;
        let (offset, s) = line
            .split_once(',')
            .ok_or(InvalidTimingPoint)
            .map_err(ParseError::curry("timing point", end_span))?;
        let (beat_length, s) = if let Some((a, b)) = s.split_once(',') {
            (a, Some(b))
        } else {
            (s, None)
        };
        let mut ret = TimingPoint {
            offset: ParseField::parse_field("timing point offset", ctx, offset)?,
            beat_length: ParseField::parse_field("timing point beat length", ctx, beat_length)?,
            time_signature: 4,
            sample_set: None,
            custom_sample_index: 0,
            sample_volume: 100,
            changes_timing: true,
            flags: TimingPointFlags::empty(),
        };
        let Some(s) = s else {
            return Ok(ret);
        };
        let (time_signature, s) = if let Some((a, b)) = s.split_once(',') {
            (a, Some(b))
        } else {
            (s, None)
        };
        if !time_signature.as_ref().starts_with('0') {
            ret.time_signature =
                ParseField::parse_field("timing point time signature", ctx, time_signature)?;
        }
        let Some(s) = s else {
            return Ok(ret);
        };
        let (sample_set, s) = if let Some((a, b)) = s.split_once(',') {
            (a, Some(b))
        } else {
            (s, None)
        };
        ret.sample_set = Some(
            i32::parse_field("timing point sample set", ctx, sample_set)?
                .try_into()
                .map_err(ParseError::curry(
                    "timing point sample set",
                    sample_set.span(),
                ))?,
        );
        let Some(s) = s else {
            return Ok(ret);
        };
        let (custom_sample_index, s) = if let Some((a, b)) = s.split_once(',') {
            (a, Some(b))
        } else {
            (s, None)
        };
        ret.custom_sample_index =
            ParseField::parse_field("timing point sample index", ctx, custom_sample_index)?;
        let Some(s) = s else {
            return Ok(ret);
        };
        let (sample_volume, s) = if let Some((a, b)) = s.split_once(',') {
            (a, Some(b))
        } else {
            (s, None)
        };
        ret.sample_volume =
            ParseField::parse_field("timing point sample volume", ctx, sample_volume)?;
        let Some(s) = s else {
            return Ok(ret);
        };
        let (changes_timing, s) = if let Some((a, b)) = s.split_once(',') {
            (a, Some(b))
        } else {
            (s, None)
        };
        ret.changes_timing = ParseField::parse_field("timing point type", ctx, changes_timing)?;
        let Some(s) = s else {
            return Ok(ret);
        };
        let (flags, _s) = if let Some((a, b)) = s.split_once(',') {
            (a, Some(b))
        } else {
            (s, None)
        };
        ret.flags = ParseField::parse_field("timing point flags", ctx, flags)?;
        Ok(ret)
    }
}

impl<'a> ParseField<'a> for HitObject<'a> {
    fn parse_field(
        _name: impl Into<Cow<'static, str>>,
        ctx: &Context,
        line: impl StaticCow<'a>,
    ) -> Result<Self, ParseError> {
        let mut end_span = line.span();
        end_span.start = end_span.end;
        let s = line;
        let (x, s) = s
            .split_once(',')
            .ok_or(InvalidHitObject)
            .map_err(ParseError::curry("hit object y", end_span))?;
        let x = f64::parse_field("hit object x", ctx, x)?;
        let x = x as i32;
        let (y, s) = s
            .split_once(',')
            .ok_or(InvalidHitObject)
            .map_err(ParseError::curry("hit object time", end_span))?;
        let y = f64::parse_field("hit object y", ctx, y)?;
        let y = y as i32;
        let (time, s) = s
            .split_once(',')
            .ok_or(InvalidHitObject)
            .map_err(ParseError::curry("hit object time", end_span))?;
        let time = f64::parse_field("hit object time", ctx, time)?;
        let time = time as i32;
        let (kind, s) = s
            .split_once(',')
            .ok_or(InvalidHitObject)
            .map_err(ParseError::curry("hit object sound", end_span))?;
        let type_span = kind.span();
        let kind = i32::parse_field("hit object type", ctx, kind)?;
        let kind = HitObjectFlags::from_bits_retain(kind);
        let (sound, s) = if let Some((a, b)) = s.split_once(',') {
            (a, Some(b))
        } else {
            (s, None)
        };
        let sound = i32::parse_field("hit object sound", ctx, sound)?;
        let sound = SoundTypes::from_bits_retain(sound);
        let combo_start = kind.contains(HitObjectFlags::COMBO_START);
        let combo_colour_skip = (kind & HitObjectFlags::COMBO_COLOUR_OFFSET_MASK).bits() >> 4;
        let (kind, s) = if kind.contains(HitObjectFlags::CIRCLE) {
            (HitObjectKind::Circle, s)
        } else if kind.contains(HitObjectFlags::SLIDER) {
            let Some(s) = s else {
                return Err(ParseError::curry("slider points", end_span)(
                    InvalidHitObject,
                ));
            };
            let (points, s) = s
                .split_once(',')
                .ok_or(InvalidHitObject)
                .map_err(ParseError::curry("slider slide count", end_span))?;
            let mut kind = SliderKind::Catmull;
            let mut curve_points = Vec::new();
            for point in points.split('|') {
                if point.as_ref().len() == 1 {
                    kind = SliderKind::try_from(point.as_ref().chars().next().unwrap())
                        .map_err(ParseError::curry("slider type", point.span()))?;
                    continue;
                }
                let (x, y) = point
                    .split_once(',')
                    .ok_or(InvalidHitObject)
                    .map_err(ParseError::curry("slider point coordinates", point.span()))?;
                let x = f64::parse_field("slider point x", ctx, x)?;
                let x = x as i32;
                let y = f64::parse_field("slider point x", ctx, y)?;
                let y = y as i32;
                curve_points.push((x, y));
            }
            let (slide_count, s) = s
                .split_once(',')
                .map(|(a, b)| (a, Some(b)))
                .unwrap_or((s, None));
            let (length, s) = if let Some(s) = s {
                let (length, s) = s
                    .split_once(',')
                    .map(|(a, b)| (a, Some(b)))
                    .unwrap_or((s, None));
                let length = ParseField::parse_field("slider length", ctx, length)?;
                (length, s)
            } else {
                (0., s)
            };
            let (mut edge_sounds, s) = if let Some(s) = s {
                let (edge_sounds, s) = s
                    .split_once(',')
                    .map(|(a, b)| (a, Some(b)))
                    .unwrap_or((s, None));
                let mut sounds = vec![];
                if !edge_sounds.as_ref().is_empty() {
                    for sound in edge_sounds.split('|') {
                        sounds.push(HitSound {
                            sounds: SoundTypes::from_bits_retain(ParseField::parse_field(
                                "slider edge sound",
                                ctx,
                                sound,
                            )?),
                            addition_set: SampleSet::None,
                            sample_set: SampleSet::None,
                        });
                    }
                }
                (sounds, s)
            } else {
                (vec![], s)
            };
            let s = if let Some(s) = s {
                let (edge_samples, s) = s
                    .split_once(',')
                    .map(|(a, b)| (a, Some(b)))
                    .unwrap_or((s, None));
                if !edge_samples.as_ref().is_empty() {
                    for (i, sound) in edge_samples.split('|').enumerate() {
                        let (sample, addition) = sound
                            .split_once(':')
                            .ok_or(InvalidHitObject)
                            .map_err(ParseError::curry("slider edge sample sets", sound.span()))?;
                        let sample = SampleSet::try_from(i32::parse_field(
                            "slider edge sample set",
                            ctx,
                            sample,
                        )?)
                        .map_err(ParseError::curry("slider edge sample set", sample.span()))?;
                        let addition = SampleSet::try_from(i32::parse_field(
                            "slider edge sample addition set",
                            ctx,
                            addition,
                        )?)
                        .map_err(ParseError::curry(
                            "slider edge sample addition set",
                            addition.span(),
                        ))?;
                        if let Some(val) = edge_sounds.get_mut(i) {
                            val.sample_set = sample;
                            val.addition_set = addition;
                        } else {
                            edge_sounds.push(HitSound {
                                sounds: SoundTypes::empty(),
                                sample_set: sample,
                                addition_set: addition,
                            });
                        }
                    }
                }
                s
            } else {
                s
            };
            let slide_count = i32::parse_field("slider slide count", ctx, slide_count)?;
            (
                HitObjectKind::Slider {
                    kind,
                    curve_points,
                    length,
                    edge_sounds,
                    slide_count,
                },
                s,
            )
        } else if kind.contains(HitObjectFlags::SPINNER) {
            let Some(s) = s else {
                return Err(ParseError::curry("spinner end time", end_span)(
                    InvalidHitObject,
                ));
            };
            let (end_time, s) = s
                .split_once(',')
                .map(|(a, b)| (a, Some(b)))
                .unwrap_or((s, None));
            let end_time = i32::parse_field("spinner end time", ctx, end_time)?;
            (HitObjectKind::Spinner { end_time }, s)
        } else if kind.contains(HitObjectFlags::HOLD_NOTE) {
            let Some(s) = s else {
                return Err(ParseError::curry("hold note end time", end_span)(
                    InvalidHitObject,
                ));
            };
            // the : is NOT a typo
            let (end_time, s) = s
                .split_once(':')
                .map(|(a, b)| (a, Some(b)))
                .unwrap_or((s, None));
            let end_time = i32::parse_field("hold note end time", ctx, end_time)?;
            (HitObjectKind::HoldNote { end_time }, s)
        } else {
            return Err(ParseError::curry("hit object type", type_span)(
                InvalidHitObject,
            ));
        };
        let mut hit_sound = FullHitSound {
            hit_sound: HitSound {
                sounds: sound,
                sample_set: SampleSet::None,
                addition_set: SampleSet::None,
            },
            custom_sample_index: 0,
            volume: 0,
            sample_file: "".into(),
        };
        if let Some(s) = s {
            let (sample, s) =
                s.split_once(':')
                    .ok_or(InvalidHitObject)
                    .map_err(ParseError::curry(
                        "hit object sample addition set",
                        end_span,
                    ))?;
            hit_sound.hit_sound.sample_set =
                SampleSet::try_from(i32::parse_field("hit object sample set", ctx, sample)?)
                    .map_err(ParseError::curry("hit object sample set", sample.span()))?;
            let (addition, s) = s
                .split_once(':')
                .map(|(a, b)| (a, Some(b)))
                .unwrap_or((s, None));
            hit_sound.hit_sound.addition_set = SampleSet::try_from(i32::parse_field(
                "hit object sample addition set",
                ctx,
                addition,
            )?)
            .map_err(ParseError::curry(
                "hit object sample addition set",
                addition.span(),
            ))?;
            let s = if let Some(s) = s {
                let (custom, s) = s
                    .split_once(':')
                    .map(|(a, b)| (a, Some(b)))
                    .unwrap_or((s, None));
                hit_sound.custom_sample_index =
                    i32::parse_field("hit object custom sample index", ctx, custom)?;
                s
            } else {
                s
            };
            let s = if let Some(s) = s {
                let (volume, s) = s
                    .split_once(':')
                    .map(|(a, b)| (a, Some(b)))
                    .unwrap_or((s, None));
                let volume = i32::parse_field("hit object sample volume", ctx, volume)?;
                hit_sound.volume = volume;
                s
            } else {
                s
            };
            let _s = if let Some(s) = s {
                let (sample_file, s) = s
                    .split_once(':')
                    .map(|(a, b)| (a, Some(b)))
                    .unwrap_or((s, None));
                hit_sound.sample_file = sample_file.into_a_cow();
                s
            } else {
                s
            };
        }
        Ok(Self {
            x,
            y,
            time,
            combo_start,
            combo_colour_skip,
            hit_sound,
            kind,
        })
    }
}

impl<'a> ParseField<'a> for Cow<'a, str> {
    fn parse_field(
        _name: impl Into<Cow<'static, str>>,
        _ctx: &Context,
        line: impl StaticCow<'a>,
    ) -> Result<Self, ParseError> {
        Ok(line.into_a_cow())
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
pub struct ParseError {
    pub field: Cow<'static, str>,
    pub span: Span,
    #[source]
    pub reason: ParseErrorReason,
}

impl ParseError {
    pub fn curry<E: Into<ParseErrorReason>>(
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
