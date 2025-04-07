use std::{
    borrow::Cow,
    ops::{Add, Sub},
};

use thiserror::Error;

use crate::{
    beatmap::{
        BeatmapSection, Context, EventSampleSet, EventTrigger, FullHitSound, HitObject,
        HitObjectFlags, HitObjectKind, HitSound, SampleSet, Section, SliderKind, SoundType,
        SoundTypes, Time, TimingPoint, TimingPointFlags,
    },
    util::StaticCow,
    ParseError,
};

use super::NoBoolValue;

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
    ) -> Result<Option<Section>, ParseError> {
        self.push(ParseField::parse_field("", ctx, line)?);
        Ok(None)
    }
}

impl<'a, T: Copy + Clone + From<i8> + Add<T, Output = T> + Sub<T, Output = T> + ParseField<'a>>
    ParseField<'a> for Time<T>
{
    fn parse_field(
        name: impl Into<Cow<'static, str>>,
        ctx: &Context,
        line: impl StaticCow<'a>,
    ) -> Result<Self, ParseError> {
        Ok(Self::new(
            ParseField::parse_field(name, ctx, line)?,
            ctx.version,
        ))
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
        let mut s = line.split(',');
        let offset = s
            .next()
            .ok_or(InvalidTimingPoint)
            .map_err(ParseError::curry("timing point offset", end_span))?;
        let beat_length = s
            .next()
            .ok_or(InvalidTimingPoint)
            .map_err(ParseError::curry("timing point beat length", end_span))?;
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
        let Some(time_signature) = s.next() else {
            return Ok(ret);
        };
        if !time_signature.as_ref().starts_with('0') {
            ret.time_signature =
                ParseField::parse_field("timing point time signature", ctx, time_signature)?;
        }
        let Some(sample_set) = s.next() else {
            return Ok(ret);
        };
        ret.sample_set = Some(
            i32::parse_field("timing point sample set", ctx, sample_set)?
                .try_into()
                .map_err(ParseError::curry(
                    "timing point sample set",
                    sample_set.span(),
                ))?,
        );
        let Some(custom_sample_index) = s.next() else {
            return Ok(ret);
        };
        ret.custom_sample_index =
            ParseField::parse_field("timing point sample index", ctx, custom_sample_index)?;
        let Some(sample_volume) = s.next() else {
            return Ok(ret);
        };
        ret.sample_volume =
            ParseField::parse_field("timing point sample volume", ctx, sample_volume)?;
        let Some(changes_timing) = s.next() else {
            return Ok(ret);
        };
        ret.changes_timing = ParseField::parse_field("timing point type", ctx, changes_timing)?;
        let Some(flags) = s.next() else {
            return Ok(ret);
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
        let mut s = line.split(',');
        let x = s
            .next()
            .ok_or(InvalidHitObject)
            .map_err(ParseError::curry("hit object x", end_span))?;
        let x = f64::parse_field("hit object x", ctx, x)?;
        let x = x as i32;
        let y = s
            .next()
            .ok_or(InvalidHitObject)
            .map_err(ParseError::curry("hit object y", end_span))?;
        let y = f64::parse_field("hit object y", ctx, y)?;
        let y = y as i32;
        let time = s
            .next()
            .ok_or(InvalidHitObject)
            .map_err(ParseError::curry("hit object time", end_span))?;
        let time = f64::parse_field("hit object time", ctx, time)?;
        let time = time as i32;
        let kind = s
            .next()
            .ok_or(InvalidHitObject)
            .map_err(ParseError::curry("hit object type", end_span))?;
        let type_span = kind.span();
        let kind = i32::parse_field("hit object type", ctx, kind)?;
        let kind = HitObjectFlags::from_bits_retain(kind);
        let sound = s
            .next()
            .ok_or(InvalidHitObject)
            .map_err(ParseError::curry("hit object sound", end_span))?;
        let sound = i32::parse_field("hit object sound", ctx, sound)?;
        let sound = SoundTypes::from_bits_retain(sound);
        let combo_start = kind.contains(HitObjectFlags::COMBO_START);
        let combo_colour_skip = (kind & HitObjectFlags::COMBO_COLOUR_OFFSET_MASK).bits() >> 4;
        let (kind, extra) = if kind.contains(HitObjectFlags::CIRCLE) {
            (HitObjectKind::Circle, s.next())
        } else if kind.contains(HitObjectFlags::SLIDER) {
            let points = s
                .next()
                .ok_or(InvalidHitObject)
                .map_err(ParseError::curry("slider points", end_span))?;
            let mut kind = SliderKind::Catmull;
            let mut curve_points = Vec::new();
            for point in points.split('|') {
                if point.as_ref().len() == 1 {
                    kind = SliderKind::try_from(point.as_ref().chars().next().unwrap())
                        .map_err(ParseError::curry("slider type", point.span()))?;
                    continue;
                }
                let (x, y) = point
                    .split_once(':')
                    .ok_or(InvalidHitObject)
                    .map_err(ParseError::curry("slider point coordinates", point.span()))?;
                let x = f64::parse_field("slider point x", ctx, x)?;
                let x = x as i32;
                let y = f64::parse_field("slider point x", ctx, y)?;
                let y = y as i32;
                curve_points.push((x, y));
            }
            let slide_count = s
                .next()
                .ok_or(InvalidHitObject)
                .map_err(ParseError::curry("slider slide count", end_span))?;
            let slide_count = i32::parse_field("slider slide count", ctx, slide_count)?;
            let length = if let Some(length) = s.next() {
                ParseField::parse_field("slider length", ctx, length)?
            } else {
                0.
            };
            let mut edge_sounds = if let Some(edge_sounds) = s.next() {
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
                sounds
            } else {
                vec![]
            };
            if let Some(edge_samples) = s.next() {
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
            }
            (
                HitObjectKind::Slider {
                    kind,
                    curve_points,
                    length,
                    edge_sounds,
                    slide_count,
                },
                s.next(),
            )
        } else if kind.contains(HitObjectFlags::SPINNER) {
            let end_time = s
                .next()
                .ok_or(InvalidHitObject)
                .map_err(ParseError::curry("spinner end time", end_span))?;
            let end_time = i32::parse_field("spinner end time", ctx, end_time)?;
            (HitObjectKind::Spinner { end_time }, s.next())
        } else if kind.contains(HitObjectFlags::HOLD_NOTE) {
            let end_time_and_extra = s
                .next()
                .ok_or(InvalidHitObject)
                .map_err(ParseError::curry("hold note end time", end_span))?;
            let (end_time, extra) = end_time_and_extra
                .split_once(':')
                .map(|(a, b)| (a, Some(b)))
                .unwrap_or((end_time_and_extra, None));
            let end_time = i32::parse_field("hold note end time", ctx, end_time)?;
            (HitObjectKind::HoldNote { end_time }, extra)
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
        if let Some(extra) = extra.filter(|x| !x.as_ref().is_empty()) {
            let mut s = extra.split(':');
            let sample = s
                .next()
                .ok_or(InvalidHitObject)
                .map_err(ParseError::curry("hit object sample set", end_span))?;
            hit_sound.hit_sound.sample_set =
                SampleSet::try_from(i32::parse_field("hit object sample set", ctx, sample)?)
                    .map_err(ParseError::curry("hit object sample set", sample.span()))?;
            let addition = s.next().ok_or(InvalidHitObject).map_err(ParseError::curry(
                "hit object sample addition set",
                end_span,
            ))?;
            hit_sound.hit_sound.addition_set = SampleSet::try_from(i32::parse_field(
                "hit object sample addition set",
                ctx,
                addition,
            )?)
            .map_err(ParseError::curry(
                "hit object sample addition set",
                addition.span(),
            ))?;
            if let Some(custom) = s.next() {
                hit_sound.custom_sample_index =
                    i32::parse_field("hit object custom sample index", ctx, custom)?;
            }
            if let Some(volume) = s.next() {
                let volume = i32::parse_field("hit object sample volume", ctx, volume)?;
                hit_sound.volume = volume;
            }
            if let Some(sample_file) = s.next() {
                hit_sound.sample_file = sample_file.into_cow();
            }
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

impl<'a> ParseField<'a> for EventTrigger {
    fn parse_field(
        name: impl Into<Cow<'static, str>>,
        _ctx: &Context,
        line: impl StaticCow<'a>,
    ) -> Result<Self, ParseError> {
        match line.as_ref() {
            "Passing" => Ok(Self::Passing),
            "Failing" => Ok(Self::Failing),
            "HitObjectHit" => Ok(Self::HitObjectHit),
            _ => {
                if let Some(s) = line.as_ref().strip_prefix("HitSound") {
                    let (set, s) = if let Some(s) = s.strip_prefix("All") {
                        (Some(EventSampleSet::All), s)
                    } else if let Some(s) = s.strip_prefix("Normal") {
                        (Some(EventSampleSet::Normal), s)
                    } else if let Some(s) = s.strip_prefix("Soft") {
                        (Some(EventSampleSet::Soft), s)
                    } else if let Some(s) = s.strip_prefix("Drum") {
                        (Some(EventSampleSet::Drum), s)
                    } else {
                        (None, s)
                    };
                    let (sample_set, addition_set, s) = if let Some(set) = set {
                        let (add, s) = if let Some(s) = s.strip_prefix("All") {
                            (Some(EventSampleSet::All), s)
                        } else if let Some(s) = s.strip_prefix("Normal") {
                            (Some(EventSampleSet::Normal), s)
                        } else if let Some(s) = s.strip_prefix("Soft") {
                            (Some(EventSampleSet::Soft), s)
                        } else if let Some(s) = s.strip_prefix("Drum") {
                            (Some(EventSampleSet::Drum), s)
                        } else {
                            (None, s)
                        };
                        (Some(set), add, s)
                    } else {
                        (None, None, s)
                    };
                    let (sound, s) = if let Some(s) = s.strip_prefix("Whistle") {
                        (Some(SoundType::Whistle), s)
                    } else if let Some(s) = s.strip_prefix("Finish") {
                        (Some(SoundType::Finish), s)
                    } else if let Some(s) = s.strip_prefix("Clap") {
                        (Some(SoundType::Clap), s)
                    } else {
                        (None, s)
                    };
                    let custom_sample_index = s.parse().ok();
                    Ok(Self::HitSound {
                        sample_set,
                        addition_set,
                        sound,
                        custom_sample_index,
                    })
                } else {
                    Err(ParseError::curry(name, line.span())(InvalidEventCommand))
                }
            }
        }
    }
}

impl std::fmt::Display for EventTrigger {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Passing => f.write_str("Passing"),
            Self::Failing => f.write_str("Failing"),
            Self::HitObjectHit => f.write_str("HitObjectHit"),
            Self::HitSound {
                sample_set,
                addition_set,
                sound,
                custom_sample_index,
            } => {
                let sample_set = if addition_set.is_some() {
                    Some(sample_set.unwrap_or_default())
                } else {
                    *sample_set
                };
                f.write_str("HitSound")?;
                for set in [sample_set, *addition_set].into_iter().flatten() {
                    write!(f, "{set}")?;
                }
                if let Some(sound) = sound {
                    write!(f, "{sound}")?;
                }
                if let Some(idx) = custom_sample_index {
                    write!(f, "{idx}")?;
                }
                Ok(())
            }
        }
    }
}

impl<'a> ParseField<'a> for Cow<'a, str> {
    fn parse_field(
        _name: impl Into<Cow<'static, str>>,
        _ctx: &Context,
        line: impl StaticCow<'a>,
    ) -> Result<Self, ParseError> {
        Ok(line.into_cow())
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
