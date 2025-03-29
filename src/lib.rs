#![allow(clippy::unit_arg)]
use bitflags::bitflags;
use osu_rs_derive::{BeatmapEnum, BeatmapSection};
use parsers::{InvalidColour, InvalidEvent, InvalidEventCommand, ParseError};
use std::{
    borrow::Cow,
    io::{self, BufRead, BufReader, Seek, Write},
    ops::{Add, Sub},
};
use util::{Borrowed, Lended, StaticCow};

mod parsers;
mod util;

use parsers::{
    CharEnumParseError, EnumParseError, IntEnumParseError, InvalidRecordField, ParseField,
    RecordParseError,
};

#[derive(Copy, Clone, Debug)]
pub struct Context {
    pub version: i32,
}

pub trait BeatmapSection<'a> {
    fn consume_line(
        &mut self,
        ctx: &Context,
        line: impl StaticCow<'a>,
    ) -> Result<Option<Section>, ParseError>;
}

use thiserror::Error;

pub type Colour = (u8, u8, u8);

/// Map countdown types
#[derive(BeatmapEnum, Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub enum Countdown {
    #[default]
    None = 0,
    Normal = 1,
    HalfSpeed = 2,
    DoubleSpeed = 3,
}

/// Hit circle overlay position compared to hit numbers
#[derive(BeatmapEnum, Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
#[beatmap_enum(ignore_case)]
pub enum OverlayPosition {
    #[default]
    NoChange = 0,
    Below = 1,
    Above = 2,
}

/// Sample sets
#[derive(BeatmapEnum, Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub enum SampleSet {
    All = -1,
    #[default]
    None = 0,
    Normal = 1,
    Soft = 2,
    Drum = 3,
}

/// Event sample sets
#[derive(BeatmapEnum, Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub enum EventSampleSet {
    #[default]
    All = -1,
    Normal = 1,
    Soft = 2,
    Drum = 3,
}

/// Game modes
#[derive(BeatmapEnum, Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum GameMode {
    Osu = 0,
    Taiko = 1,
    CatchTheBeat = 2,
    Mania = 3,
}

/// Time in milliseconds
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Time<T>(pub T);

impl<T: Copy + Clone + From<i8> + Add<T, Output = T> + Sub<T, Output = T>> Time<T> {
    pub fn new(time: T, version: i32) -> Self {
        Self(if version < 5 {
            time + T::from(24)
        } else {
            time
        })
    }
    pub fn serialize(&self, version: i32) -> T {
        if version < 5 {
            self.0 - T::from(24)
        } else {
            self.0
        }
    }
}

/// General section
#[derive(BeatmapSection, Clone, Debug)]
#[beatmap_section(Self::extra_key_handler)]
pub struct General<'a> {
    /// Audio file name
    pub audio_filename: Cow<'a, str>,
    /// Milliseconds of silence before the audio starts playing
    pub audio_lead_in: i32,
    /// md5 hash of the audio file. Deprecated and never serialized
    pub audio_hash: Cow<'a, str>,
    /// Preview time
    pub preview_time: i32,
    /// Countdown type
    #[from(i32)]
    pub countdown: Countdown,
    pub sample_set: SampleSet,
    // #[clamp(0.0, 1.0)]
    pub stack_leniency: f32,
    #[from(i32)]
    pub mode: GameMode,
    pub letterbox_in_breaks: bool,
    pub story_fire_in_front: bool,
    pub use_skin_sprites: bool,
    pub always_show_playfield: bool,
    pub custom_samples: bool,
    pub overlay_position: OverlayPosition,
    pub skin_preference: Cow<'a, str>,
    pub epilepsy_warning: bool,
    // #[clamp(0, 3)]
    pub countdown_offset: i32,
    pub special_style: bool,
    pub widescreen_storyboard: bool,
    pub samples_match_playback_rate: bool,
}

impl<'a> General<'a> {
    fn extra_key_handler(k: impl StaticCow<'a>) -> Option<Section> {
        k.as_ref().starts_with("Editor").then_some(Section::Editor)
    }
}

impl<'a> General<'a> {
    fn default_with_context(ctx: &Context) -> Self {
        Self {
            audio_filename: "".into(),
            audio_lead_in: 0,
            audio_hash: "".into(),
            preview_time: -1,
            countdown: Countdown::Normal,
            sample_set: SampleSet::Normal,
            stack_leniency: 0.7,
            mode: GameMode::Osu,
            letterbox_in_breaks: false,
            story_fire_in_front: true,
            use_skin_sprites: false,
            always_show_playfield: false,
            custom_samples: ctx.version < 4,
            overlay_position: OverlayPosition::NoChange,
            skin_preference: "".into(),
            epilepsy_warning: false,
            countdown_offset: 0,
            special_style: false,
            widescreen_storyboard: false,
            samples_match_playback_rate: false,
        }
    }
}

/// Editor section
#[derive(BeatmapSection, Clone, Debug)]
pub struct Editor {
    // aliases don't occur in the Editor section itself they come from the General secction
    #[alias("EditorBookmarks")]
    pub bookmarks: Vec<i32>,
    #[alias("EditorDistanceSpacing")]
    pub distance_spacing: f64,
    pub beat_divisor: i32,
    pub grid_size: i32,
    pub timeline_zoom: f32,
}

impl Editor {
    fn default_with_context(_ctx: &Context) -> Self {
        Self {
            bookmarks: vec![],
            distance_spacing: 0.8,
            beat_divisor: 1,
            grid_size: 32,
            timeline_zoom: 1.,
        }
    }
}

/// Metadata section
#[derive(BeatmapSection, Clone, Debug)]
pub struct Metadata<'a> {
    pub title: Cow<'a, str>,
    pub title_unicode: Cow<'a, str>,
    pub artist: Cow<'a, str>,
    pub artist_unicode: Cow<'a, str>,
    pub creator: Cow<'a, str>,
    pub version: Cow<'a, str>,
    pub source: Cow<'a, str>,
    pub tags: Cow<'a, str>,
    pub beatmap_id: i32,
    pub beatmap_set_id: i32,
}

impl<'a> Metadata<'a> {
    fn default_with_context(_ctx: &Context) -> Self {
        Self {
            title: "".into(),
            title_unicode: "".into(),
            artist: "".into(),
            artist_unicode: "".into(),
            creator: "".into(),
            version: "".into(),
            source: "".into(),
            tags: "".into(),
            beatmap_id: 0,
            beatmap_set_id: -1,
        }
    }
}

/// Difficulty section
#[derive(BeatmapSection, Copy, Clone, Debug)]
pub struct Difficulty {
    pub hp_drain_rate: f32,
    pub circle_size: f32,
    pub overall_difficulty: f32,
    pub approach_rate: f32,
    pub slider_multiplier: f64,
    pub slider_tick_rate: f64,
}

impl Difficulty {
    fn default_with_context(_ctx: &Context) -> Self {
        Self {
            hp_drain_rate: 5.,
            circle_size: 5.,
            overall_difficulty: 5.,
            approach_rate: 5.,
            slider_multiplier: 1.4,
            slider_tick_rate: 1.,
        }
    }
}

/// Colours section
#[derive(Clone, Debug)]
pub struct Colours<'a> {
    pub colours: Vec<(Cow<'a, str>, Colour)>,
}

impl<'a> Colours<'a> {
    fn default_with_context(_ctx: &Context) -> Self {
        Self {
            colours: Vec::new(),
        }
    }
}

impl<'a> BeatmapSection<'a> for Colours<'a> {
    fn consume_line(
        &mut self,
        ctx: &Context,
        line: impl StaticCow<'a>,
    ) -> Result<Option<Section>, ParseError> {
        if let Some((key, value)) = line.split_once(':') {
            let key = key.trim();
            let value = value.trim();
            let mut end_span = value.span();
            end_span.start = end_span.end;
            let (r, value) = value
                .split_once(',')
                .ok_or(InvalidColour)
                .map_err(ParseError::curry("colour", end_span))?;
            let (g, b) = value
                .split_once(',')
                .ok_or(InvalidColour)
                .map_err(ParseError::curry("colour", end_span))?;
            let r = ParseField::parse_field("red colour channel", ctx, r)?;
            let g = ParseField::parse_field("green colour channel", ctx, g)?;
            let b = ParseField::parse_field("blue colour channel", ctx, b)?;
            self.colours.push((key.into_cow(), (r, g, b)));
        }
        Ok(None)
    }
}

/// Variables section
#[derive(Clone, Debug)]
pub struct Variables<'a> {
    pub variables: Vec<(Cow<'a, str>, Cow<'a, str>)>,
}

impl<'a> Variables<'a> {
    fn default_with_context(_ctx: &Context) -> Self {
        Self {
            variables: Vec::new(),
        }
    }
}

impl<'a> BeatmapSection<'a> for Variables<'a> {
    fn consume_line(
        &mut self,
        _ctx: &Context,
        line: impl StaticCow<'a>,
    ) -> Result<Option<Section>, ParseError> {
        if let Some((key, value)) = line.split_once('=') {
            self.variables.push((key.into_cow(), value.into_cow()));
        }
        Ok(None)
    }
}

#[derive(BeatmapEnum, Clone, Copy, PartialEq, Eq, Hash)]
enum EventId {
    Background = 0,
    Video = 1,
    Break = 2,
    Colour = 3,
    Sprite = 4,
    Sample = 5,
    Animation = 6,
}

#[derive(BeatmapEnum, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum EventOrigin {
    TopLeft = 0,
    Centre = 1,
    CentreLeft = 2,
    TopRight = 3,
    BottomCentre = 4,
    TopCentre = 5,
    Custom = 6,
    CentreRight = 7,
    BottomLeft = 8,
    BottomRight = 9,
}

#[derive(BeatmapEnum, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum EventLayer {
    Background = 0,
    // used to be Failing
    Fail = 1,
    // used to be Passing
    Pass = 2,
    Foreground = 3,
    Overlay = 4,
}

#[derive(BeatmapEnum, Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub enum EventLoop {
    #[default]
    LoopForever = 0,
    LoopOnce = 1,
}

#[derive(BeatmapEnum, Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub enum EventEasing {
    #[default]
    Linear = 0,
    LinearOut = 1,
    LinearIn = 2,
    QuadIn = 3,
    QuadOut = 4,
    QuadInOut = 5,
    CubicIn = 6,
    CubicOut = 7,
    CubicInOut = 8,
    QuartIn = 9,
    QuartOut = 10,
    QuartInOut = 11,
    QuintiIn = 12,
    QuintOut = 13,
    QuintInOut = 14,
    SineIn = 15,
    SineOut = 16,
    SineInOut = 17,
    ExpoIn = 18,
    ExpoOut = 19,
    ExpoInOut = 20,
    CircIn = 21,
    CircOut = 22,
    CircInOut = 23,
    ElasticIn = 24,
    ElasticOut = 25,
    ElasticHalfOut = 26,
    ElasticQuarterOut = 27,
    ElasticInOut = 28,
    BackIn = 29,
    BackOut = 30,
    BackInOut = 31,
    BounceIn = 32,
    BounceOut = 33,
    BounceInOut = 34,
}

#[derive(BeatmapEnum, Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[beatmap_enum(from_char)]
pub enum ParameterCommand {
    HorizontalFlip,
    VerticalFlip,
    AdditiveBlend,
}

#[derive(Clone, Copy, Debug)]
pub enum EventTrigger {
    Passing,
    Failing,
    HitObjectHit,
    HitSound {
        sample_set: Option<EventSampleSet>,
        addition_set: Option<EventSampleSet>,
        sound: Option<SoundType>,
        custom_sample_index: Option<i32>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum EventObject<'a> {
    Background {
        filename: Cow<'a, str>,
        time: Time<i32>,
        x: i32,
        y: i32,
    },
    Video {
        filename: Cow<'a, str>,
        time: Time<i32>,
        x: i32,
        y: i32,
    },
    Break {
        time: Time<i32>,
        end_time: Time<i32>,
    },
    Colour {
        time: Time<i32>,
        colour: Colour,
    },
    Sprite {
        filename: Cow<'a, str>,
        x: f64,
        y: f64,
        origin: EventOrigin,
        layer: EventLayer,
    },
    Sample {
        filename: Cow<'a, str>,
        time: Time<i32>,
        volume: f64,
        layer: EventLayer,
    },
    Animation {
        filename: Cow<'a, str>,
        x: f64,
        y: f64,
        origin: EventOrigin,
        layer: EventLayer,
        frame_count: i32,
        frame_delay: f64,
        loop_type: EventLoop,
    },
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct MoveCommand {
    pub pos: (f32, f32),
    pub end_pos: Option<(f32, f32)>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct MoveXCommand {
    pub x: f32,
    pub end_x: Option<f32>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct MoveYCommand {
    pub y: f32,
    pub end_y: Option<f32>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct FadeCommand {
    pub opacity: f32,
    pub end_opacity: Option<f32>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct RotateCommand {
    pub rotation: f32,
    pub end_rotation: Option<f32>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ScaleCommand {
    pub scale: f32,
    pub end_scale: Option<f32>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct VectorScaleCommand {
    pub scale: (f32, f32),
    pub end_scale: Option<(f32, f32)>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ColourCommand {
    pub colour: Colour,
    pub end_colour: Option<Colour>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoopCommand {
    pub time: Time<i32>,
    pub count: i32,
}

#[derive(Clone, Debug)]
pub struct TriggerCommand {
    pub trigger: EventTrigger,
    pub time_range: Option<(Time<i32>, Time<i32>)>,
    pub trigger_group: Option<i32>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BasicEventCommandSequence {
    Move(Vec<MoveCommand>),
    MoveX(Vec<MoveXCommand>),
    MoveY(Vec<MoveYCommand>),
    Fade(Vec<FadeCommand>),
    Rotate(Vec<RotateCommand>),
    Scale(Vec<ScaleCommand>),
    VectorScale(Vec<VectorScaleCommand>),
    Colour(Vec<ColourCommand>),
    Parameter(Vec<ParameterCommand>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct BasicCommand {
    pub sequence: BasicEventCommandSequence,
    pub easing: EventEasing,
    pub start_time: Time<i32>,
    /// Defaults to `start_time`
    pub end_time: Option<Time<i32>>,
}

#[derive(Clone, Debug)]
pub enum EventCommandSequence {
    Basic(BasicCommand),
    Loop(Vec<LoopCommand>, Vec<BasicCommand>),
    Trigger(Vec<TriggerCommand>, Vec<BasicCommand>),
}

impl BasicCommand {
    fn write<W: Write>(&self, ctx: &Context, f: &mut W) -> io::Result<()> {
        let common = |f: &mut W| -> io::Result<()> {
            if let Some(end_time) = self.end_time {
                write!(
                    f,
                    ",{},{},{}",
                    self.easing as i32,
                    self.start_time.serialize(ctx.version),
                    end_time.serialize(ctx.version)
                )?;
            } else {
                write!(
                    f,
                    ",{},{},",
                    self.easing as i32,
                    self.start_time.serialize(ctx.version)
                )?;
            }
            Ok(())
        };
        macro_rules! match_kind {
            ($i:tt 1 $seq:tt { $f1:ident, $f2:ident }) => {{
                f.write_all($i)?;
                common(f)?;
                for (i, elem) in $seq.iter().enumerate() {
                    let a = elem.$f1;
                    let b = elem
                        .$f2
                        .or_else(|| if i + 1 == $seq.len() { None } else { Some(a) });
                    if let Some(b) = b {
                        write!(f, ",{a},{b}")?;
                    } else {
                        write!(f, ",{a}")?;
                    }
                }
            }};
            ($i:tt 2 $seq:tt { $f1:ident, $f2:ident }) => {{
                f.write_all($i)?;
                common(f)?;
                for (i, elem) in $seq.iter().enumerate() {
                    let a = elem.$f1;
                    let b = elem
                        .$f2
                        .or_else(|| if i + 1 == $seq.len() { None } else { Some(a) });
                    if let Some(b) = b {
                        write!(f, ",{},{},{},{}", a.0, a.1, b.0, b.1)?;
                    } else {
                        write!(f, ",{},{}", a.0, a.1)?;
                    }
                }
            }};
            ($i:tt 3 $seq:tt { $f1:ident, $f2:ident }) => {{
                f.write_all($i)?;
                common(f)?;
                for (i, elem) in $seq.iter().enumerate() {
                    let a = elem.$f1;
                    let b = elem
                        .$f2
                        .or_else(|| if i + 1 == $seq.len() { None } else { Some(a) });
                    if let Some(b) = b {
                        write!(f, ",{},{},{},{},{},{}", a.0, a.1, a.2, b.0, b.1, b.2)?;
                    } else {
                        write!(f, ",{},{},{}", a.0, a.1, a.2)?;
                    }
                }
            }};
        }
        match &self.sequence {
            BasicEventCommandSequence::Move(seq) => match_kind!(b"M" 2 seq { pos, end_pos }),
            BasicEventCommandSequence::MoveX(seq) => match_kind!(b"MX" 1 seq { x, end_x }),
            BasicEventCommandSequence::MoveY(seq) => match_kind!(b"MY" 1 seq { y, end_y }),
            BasicEventCommandSequence::Fade(seq) => {
                match_kind!(b"F" 1 seq { opacity, end_opacity })
            }
            BasicEventCommandSequence::Rotate(seq) => {
                match_kind!(b"R" 1 seq { rotation, end_rotation })
            }
            BasicEventCommandSequence::Scale(seq) => match_kind!(b"S" 1 seq { scale, end_scale }),
            BasicEventCommandSequence::VectorScale(seq) => {
                match_kind!(b"V" 2 seq { scale, end_scale })
            }
            BasicEventCommandSequence::Colour(seq) => {
                match_kind!(b"C" 3 seq { colour, end_colour })
            }
            BasicEventCommandSequence::Parameter(seq) => {
                f.write_all(b"P")?;
                common(f)?;
                for param in seq {
                    f.write_all(match param {
                        ParameterCommand::VerticalFlip => b",V",
                        ParameterCommand::HorizontalFlip => b",H",
                        ParameterCommand::AdditiveBlend => b",A",
                    })?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct EventSequence<'a> {
    pub object: EventObject<'a>,
    pub commands: Vec<EventCommandSequence>,
}

/// Events section
#[derive(Clone, Debug)]
pub struct Events<'a> {
    pub events: Vec<EventSequence<'a>>,
}

impl<'a> Events<'a> {
    fn default_with_context(_ctx: &Context) -> Self {
        Self { events: Vec::new() }
    }
}

impl<'a> BeatmapSection<'a> for Events<'a> {
    fn consume_line(
        &mut self,
        ctx: &Context,
        line: impl StaticCow<'a>,
    ) -> Result<Option<Section>, ParseError> {
        if line.as_ref().starts_with("//") {
            return Ok(None);
        }
        if line.as_ref().starts_with(|x| matches!(x, ' ' | '_')) {
            let nested = matches!(line.as_ref().chars().nth(1), Some(' ' | '_'));
            let line = line.trim_matches2(' ', '_');
            let mut s = line.split(',');
            let mut end_span = line.span();
            end_span.start = end_span.end;
            let kind = s
                .next()
                .ok_or(InvalidEventCommand)
                .map_err(ParseError::curry("event command", end_span))?;
            let is_basic = !matches!(kind.as_ref(), "L" | "T");
            if is_basic {
                let easing = s
                    .next()
                    .ok_or(InvalidEventCommand)
                    .map_err(ParseError::curry("event command easing", end_span))?;
                let easing = ParseField::parse_field("event command easing", ctx, easing)?;
                let start_time = s
                    .next()
                    .ok_or(InvalidEventCommand)
                    .map_err(ParseError::curry("event command start time", end_span))?;
                let start_time =
                    ParseField::parse_field("event command start time", ctx, start_time)?;
                let end_time = s
                    .next()
                    .ok_or(InvalidEventCommand)
                    .map_err(ParseError::curry("event command end time", end_span))?;
                let end_time = if end_time.as_ref().is_empty() {
                    None
                } else {
                    Some(ParseField::parse_field(
                        "event command end time",
                        ctx,
                        end_time,
                    )?)
                };
                macro_rules! match_kind {
                    ($i:ident 1 $t:tt { $f1:ident: $d1:expr, $f2:ident: $d2:expr, }) => {{
                        let mut sequence = vec![];
                        while let Some(start) = s.next() {
                            let start = ParseField::parse_field($d1, ctx, start)?;
                            let end = s
                                .next()
                                .map(|end| ParseField::parse_field($d2, ctx, end))
                                .transpose()?;
                            sequence.push($t {
                                $f1: start,
                                $f2: end,
                            });
                        }
                        BasicEventCommandSequence::$i(sequence)
                    }};
                    ($i:ident 2 $t:tt { $f1:ident: $d11:expr, $d12:expr, $f2:ident: $d21:expr, $d22:expr, }) => {{
                        let mut sequence = vec![];
                        while let Some(start1) = s.next() {
                            let start1 = ParseField::parse_field($d11, ctx, start1)?;
                            let start2 = s
                                .next()
                                .ok_or(InvalidEventCommand)
                                .map_err(ParseError::curry($d12, end_span))?;
                            let start2 = ParseField::parse_field($d12, ctx, start2)?;
                            let end = if let Some(end1) = s.next() {
                                let end1 = ParseField::parse_field($d21, ctx, end1)?;
                                let end2 = s
                                    .next()
                                    .ok_or(InvalidEventCommand)
                                    .map_err(ParseError::curry($d22, end_span))?;
                                let end2 = ParseField::parse_field($d22, ctx, end2)?;
                                Some((end1, end2))
                            } else {
                                None
                            };
                            sequence.push($t {
                                $f1: (start1, start2),
                                $f2: end,
                            });
                        }
                        BasicEventCommandSequence::$i(sequence)
                    }};
                    ($i:ident 3 $t:tt { $f1:ident: $d11:expr, $d12:expr, $d13:expr, $f2:ident: $d21:expr, $d22:expr, $d23:expr, }) => {{
                        let mut sequence = vec![];
                        while let Some(start1) = s.next() {
                            let start1 = ParseField::parse_field($d11, ctx, start1)?;
                            let start2 = s
                                .next()
                                .ok_or(InvalidEventCommand)
                                .map_err(ParseError::curry($d12, end_span))?;
                            let start2 = ParseField::parse_field($d12, ctx, start2)?;
                            let start3 = s
                                .next()
                                .ok_or(InvalidEventCommand)
                                .map_err(ParseError::curry($d13, end_span))?;
                            let start3 = ParseField::parse_field($d13, ctx, start3)?;
                            let end = if let Some(end1) = s.next() {
                                let end1 = ParseField::parse_field($d21, ctx, end1)?;
                                let end2 = s
                                    .next()
                                    .ok_or(InvalidEventCommand)
                                    .map_err(ParseError::curry($d22, end_span))?;
                                let end2 = ParseField::parse_field($d22, ctx, end2)?;
                                let end3 = s
                                    .next()
                                    .ok_or(InvalidEventCommand)
                                    .map_err(ParseError::curry($d23, end_span))?;
                                let end3 = ParseField::parse_field($d23, ctx, end3)?;
                                Some((end1, end2, end3))
                            } else {
                                None
                            };
                            sequence.push($t {
                                $f1: (start1, start2, start3),
                                $f2: end,
                            });
                        }
                        BasicEventCommandSequence::$i(sequence)
                    }};
                }
                let sequence = match kind.as_ref() {
                    "M" => match_kind!(Move 2 MoveCommand {
                        pos: "move event command start x position", "move event command start y position",
                        end_pos: "move event command end x position", "move event command end y position",
                    }),
                    "MX" => match_kind!(MoveX 1 MoveXCommand {
                        x: "move x event command start x position",
                        end_x: "move x event command end x position",
                    }),
                    "MY" => match_kind!(MoveY 1 MoveYCommand {
                        y: "move y event command start y position",
                        end_y: "move y event command end y position",
                    }),
                    "F" => match_kind!(Fade 1 FadeCommand {
                        opacity: "fade event command start opacity",
                        end_opacity: "fade event command end opacity",
                    }),
                    "R" => match_kind!(Rotate 1 RotateCommand {
                        rotation: "rotate event command start rotation",
                        end_rotation: "rotate event command end rotation",
                    }),
                    "S" => match_kind!(Scale 1 ScaleCommand {
                        scale: "scale event command start scale",
                        end_scale: "scale event command end scale",
                    }),
                    "V" => match_kind!(VectorScale 2 VectorScaleCommand {
                        scale: "vector scale event command start x scale", "vector scale event command start y scale",
                        end_scale: "vector scale event command end x scale", "vector scale event command end y scale",
                    }),
                    "C" => match_kind!(Colour 3 ColourCommand {
                        colour: "colour event command start red channel", "colour event command start green channel", "colour event command start blue channel",
                        end_colour: "colour event command end red channel", "colour event command end green channel", "colour event command end blue channel",
                    }),
                    "P" => {
                        let mut sequence = vec![];
                        for parameter in s {
                            sequence.push(match parameter.as_ref() {
                                "H" => ParameterCommand::HorizontalFlip,
                                "V" => ParameterCommand::VerticalFlip,
                                "A" => ParameterCommand::AdditiveBlend,
                                _ => {
                                    return Err(ParseError::curry(
                                        "parameter event command parameter",
                                        parameter.span(),
                                    )(
                                        InvalidEventCommand
                                    ))
                                }
                            });
                        }
                        BasicEventCommandSequence::Parameter(sequence)
                    }
                    _ => {
                        return Err(ParseError::curry("event command type", kind.span())(
                            InvalidEventCommand,
                        ));
                    }
                };
                let cmd = BasicCommand {
                    sequence,
                    easing,
                    start_time,
                    end_time,
                };
                if let Some(event) = self.events.last_mut() {
                    match event.commands.last_mut() {
                        Some(EventCommandSequence::Loop(_, cmds)) if nested => {
                            cmds.push(cmd);
                        }
                        Some(EventCommandSequence::Trigger(_, cmds)) if nested => {
                            cmds.push(cmd);
                        }
                        _ => event.commands.push(EventCommandSequence::Basic(cmd)),
                    }
                }
            } else {
                let cmd = match kind.as_ref() {
                    "L" => {
                        let mut sequence = vec![];
                        while let Some(time) = s.next() {
                            let time =
                                ParseField::parse_field("loop event command time", ctx, time)?;
                            let count = s
                                .next()
                                .ok_or(InvalidEventCommand)
                                .map_err(ParseError::curry("loop event command count", end_span))?;
                            let count =
                                ParseField::parse_field("loop event command count", ctx, count)?;
                            sequence.push(LoopCommand { time, count });
                        }
                        EventCommandSequence::Loop(sequence, vec![])
                    }
                    "T" => {
                        let mut sequence = vec![];
                        while let Some(trigger) = s.next() {
                            let trigger = ParseField::parse_field(
                                "trigger event command trigger",
                                ctx,
                                trigger,
                            )?;
                            let time_range = if let Some(start_time) = s.next() {
                                let start_time = ParseField::parse_field(
                                    "trigger event command start time",
                                    ctx,
                                    start_time,
                                )?;
                                let end_time = s.next().ok_or(InvalidEventCommand).map_err(
                                    ParseError::curry("trigger event command end time", end_span),
                                )?;
                                let end_time = ParseField::parse_field(
                                    "trigger event command end time",
                                    ctx,
                                    end_time,
                                )?;
                                Some((start_time, end_time))
                            } else {
                                None
                            };
                            let trigger_group = s
                                .next()
                                .map(|x| {
                                    ParseField::parse_field(
                                        "trigger event command trigger group",
                                        ctx,
                                        x,
                                    )
                                })
                                .transpose()?;
                            sequence.push(TriggerCommand {
                                trigger,
                                time_range,
                                trigger_group,
                            });
                        }
                        EventCommandSequence::Trigger(sequence, vec![])
                    }
                    _ => {
                        return Err(ParseError::curry("event command type", kind.span())(
                            InvalidEventCommand,
                        ));
                    }
                };
                if let Some(event) = self.events.last_mut() {
                    event.commands.push(cmd);
                }
            }
            return Ok(None);
        }
        let (kind, s) = line
            .split_once(',')
            .ok_or(InvalidEvent)
            .map_err(ParseError::curry("event", line.span()))?;
        let mut end_span = line.span();
        end_span.start = end_span.end;
        let object = match EventId::parse_field("event type", ctx, kind)? {
            EventId::Background => {
                let (time, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("background event filename", end_span))?;
                let time = ParseField::parse_field("background event time", ctx, time)?;
                let (filename, s) = s
                    .split_once(',')
                    .map(|(a, b)| (a, Some(b)))
                    .unwrap_or((s, None));
                let filename = filename.trim_matches('"').into_cow();
                let (x, y) = if let Some((x, y)) = s.and_then(|x| x.split_once(',')) {
                    let y = y.split_once(',').map(|x| x.0).unwrap_or(y);
                    let x = ParseField::parse_field("background event position x", ctx, x)?;
                    let y = ParseField::parse_field("background event position y", ctx, y)?;
                    (x, y)
                } else {
                    (0, 0)
                };
                EventObject::Background {
                    filename,
                    time,
                    x,
                    y,
                }
            }
            EventId::Video => {
                let (time, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("video event filename", end_span))?;
                let time = ParseField::parse_field("video event time", ctx, time)?;
                let (filename, s) = s
                    .split_once(',')
                    .map(|(a, b)| (a, Some(b)))
                    .unwrap_or((s, None));
                let filename = filename.trim_matches('"').into_cow();
                let (x, y) = if let Some((x, y)) = s.and_then(|x| x.split_once(',')) {
                    let y = y.split_once(',').map(|x| x.0).unwrap_or(y);
                    let x = ParseField::parse_field("video event position x", ctx, x)?;
                    let y = ParseField::parse_field("video event position y", ctx, y)?;
                    (x, y)
                } else {
                    (0, 0)
                };
                EventObject::Video {
                    filename,
                    time,
                    x,
                    y,
                }
            }
            EventId::Break => {
                let (time, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("break event end time", end_span))?;
                let (end_time, _s) = s
                    .split_once(',')
                    .map(|(a, b)| (a, Some(b)))
                    .unwrap_or((s, None));
                let time = ParseField::parse_field("break event time", ctx, time)?;
                let end_time = ParseField::parse_field("break event time", ctx, end_time)?;
                EventObject::Break { time, end_time }
            }
            EventId::Colour => {
                let (time, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("colour event red channel", end_span))?;
                let time = ParseField::parse_field("colour event time", ctx, time)?;
                let (r, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("colour event green channel", end_span))?;
                let r = ParseField::parse_field("colour event red channel", ctx, r)?;
                let (g, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("colour event blue channel", end_span))?;
                let g = ParseField::parse_field("colour event green channel", ctx, g)?;
                let (b, _s) = s
                    .split_once(',')
                    .map(|(a, b)| (a, Some(b)))
                    .unwrap_or((s, None));
                let b = ParseField::parse_field("colour event blue channel", ctx, b)?;
                EventObject::Colour {
                    time,
                    colour: (r, g, b),
                }
            }
            EventId::Sprite => {
                let (layer, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("sprite event origin", end_span))?;
                let layer = ParseField::parse_field("sprite event layer", ctx, layer)?;
                let (origin, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("sprite event filename", end_span))?;
                let origin = ParseField::parse_field("sprite event origin", ctx, origin)?;
                let (filename, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("sprite event position x", end_span))?;
                let filename = filename.trim_matches('"').into_cow();
                let (x, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("sprite event position y", end_span))?;
                let x = ParseField::parse_field("sprite event position x", ctx, x)?;
                let (y, _s) = s
                    .split_once(',')
                    .map(|(a, b)| (a, Some(b)))
                    .unwrap_or((s, None));
                let y = ParseField::parse_field("sprite event position y", ctx, y)?;
                EventObject::Sprite {
                    filename,
                    x,
                    y,
                    origin,
                    layer,
                }
            }
            EventId::Sample => {
                let (time, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("sample event layer", end_span))?;
                let time = ParseField::parse_field("sample event time", ctx, time)?;
                let (layer, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("sample event filename", end_span))?;
                let layer = ParseField::parse_field("sample event layer", ctx, layer)?;
                let (filename, s) = s
                    .split_once(',')
                    .map(|(a, b)| (a, Some(b)))
                    .unwrap_or((s, None));
                let filename = filename.trim_matches('"').into_cow();
                let (volume, _s) = if let Some(s) = s {
                    let (volume, s) = s
                        .split_once(',')
                        .map(|(a, b)| (a, Some(b)))
                        .unwrap_or((s, None));
                    let volume = ParseField::parse_field("sample event volume", ctx, volume)?;
                    (volume, s)
                } else {
                    (100., s)
                };
                EventObject::Sample {
                    filename,
                    time,
                    volume,
                    layer,
                }
            }
            EventId::Animation => {
                let (layer, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("animation event origin", end_span))?;
                let layer = ParseField::parse_field("animation event layer", ctx, layer)?;
                let (origin, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("animation event filename", end_span))?;
                let origin = ParseField::parse_field("animation event origin", ctx, origin)?;
                let (filename, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("animation event position x", end_span))?;
                let filename = filename.trim_matches('"').into_cow();
                let (x, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("animation event position y", end_span))?;
                let x = ParseField::parse_field("animation event position x", ctx, x)?;
                let (y, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("animation event frame count", end_span))?;
                let y = ParseField::parse_field("animation event position y", ctx, y)?;
                let (frame_count, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("animation event frame delay", end_span))?;
                let frame_count =
                    ParseField::parse_field("animation event frame count", ctx, frame_count)?;
                let (frame_delay, s) = s
                    .split_once(',')
                    .map(|(a, b)| (a, Some(b)))
                    .unwrap_or((s, None));
                let frame_delay =
                    ParseField::parse_field("animation event frame delay", ctx, frame_delay)?;
                let (loop_type, _s) = if let Some(s) = s {
                    let (loop_type, s) = s
                        .split_once(',')
                        .map(|(a, b)| (a, Some(b)))
                        .unwrap_or((s, None));
                    let loop_type =
                        ParseField::parse_field("animation event loop type", ctx, loop_type)?;
                    (loop_type, s)
                } else {
                    (EventLoop::default(), s)
                };
                EventObject::Animation {
                    filename,
                    x,
                    y,
                    origin,
                    layer,
                    frame_count,
                    frame_delay,
                    loop_type,
                }
            }
        };
        self.events.push(EventSequence {
            object,
            commands: vec![],
        });
        Ok(None)
    }
}

bitflags! {
    #[derive(Clone, Debug, Default)]
    pub struct TimingPointFlags: i32 {
        const KIAI = 1;
        const OMIT_FIRST_BARLINE = 8;
    }
}

impl std::str::FromStr for TimingPointFlags {
    type Err = std::num::ParseIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::from_bits_retain(s.parse()?))
    }
}

#[derive(Clone, Debug)]
pub struct TimingPoint {
    pub offset: Time<f64>,
    pub beat_length: f64,
    /// e.g. 4 or 3
    pub time_signature: i32,
    pub sample_set: Option<SampleSet>,
    pub custom_sample_index: i32,
    pub sample_volume: i32,
    pub changes_timing: bool,
    pub flags: TimingPointFlags,
}

#[derive(BeatmapEnum, Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
#[beatmap_enum(from_char)]
pub enum SliderKind {
    Linear = 0,
    Perfect = 1,
    Bezier = 2,
    #[default]
    Catmull = 3,
}

bitflags! {
    #[derive(Copy, Clone, Debug, Default)]
    pub struct SoundTypes: i32 {
        const NORMAL = 1;
        const WHISTLE = 2;
        const FINISH = 4;
        const CLAP = 8;
    }
}

#[derive(BeatmapEnum, Copy, Clone, Debug, PartialEq, Eq)]
pub enum SoundType {
    Whistle,
    Finish,
    Clap,
}

#[derive(Copy, Clone, Debug, Default)]
pub struct HitSound {
    pub sounds: SoundTypes,
    pub sample_set: SampleSet,
    pub addition_set: SampleSet,
}

#[derive(Clone, Debug, Default)]
pub struct FullHitSound<'a> {
    pub hit_sound: HitSound,
    pub custom_sample_index: i32,
    pub volume: i32,
    pub sample_file: Cow<'a, str>,
}

bitflags! {
    #[derive(Copy, Clone, Debug, Default)]
    pub struct HitObjectFlags: i32 {
        const CIRCLE = 1;
        const SLIDER = 2;
        const COMBO_START = 4;
        const SPINNER = 8;
        const COMBO_COLOUR_OFFSET_MASK = 0b1110000;
        const HOLD_NOTE = 128;
    }
}

#[derive(Clone, Debug)]
pub enum HitObjectKind {
    Circle,
    Slider {
        kind: SliderKind,
        curve_points: Vec<(i32, i32)>,
        length: f64,
        edge_sounds: Vec<HitSound>,
        slide_count: i32,
    },
    Spinner {
        end_time: i32,
    },
    HoldNote {
        end_time: i32,
    },
}

#[derive(Clone, Debug)]
pub struct HitObject<'a> {
    pub x: i32,
    pub y: i32,
    pub time: i32,
    pub combo_start: bool,
    pub combo_colour_skip: i32,
    pub hit_sound: FullHitSound<'a>,
    pub kind: HitObjectKind,
}

impl<'a> HitObject<'a> {
    pub fn flags(&self) -> HitObjectFlags {
        let mut ret = match self.kind {
            HitObjectKind::Circle => HitObjectFlags::CIRCLE,
            HitObjectKind::Slider { .. } => HitObjectFlags::SLIDER,
            HitObjectKind::Spinner { .. } => HitObjectFlags::SPINNER,
            HitObjectKind::HoldNote { .. } => HitObjectFlags::HOLD_NOTE,
        };
        if self.combo_start {
            ret |= HitObjectFlags::COMBO_START;
        }
        ret |= HitObjectFlags::COMBO_COLOUR_OFFSET_MASK
            & HitObjectFlags::from_bits_retain(self.combo_colour_skip << 4);
        ret
    }
}

#[derive(Debug, Error)]
pub enum ReadError {
    #[error("{0}")]
    Io(
        #[from]
        #[source]
        io::Error,
    ),
    #[error("{0}")]
    Parse(
        #[from]
        #[source]
        ParseError,
    ),
}

impl<'a> BeatmapSection<'a> for () {
    fn consume_line(
        &mut self,
        _ctx: &Context,
        _line: impl StaticCow<'a>,
    ) -> Result<Option<Section>, ParseError> {
        Ok(None)
    }
}

#[derive(BeatmapEnum, Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Section {
    General,
    Colours,
    Editor,
    Metadata,
    TimingPoints,
    Events,
    HitObjects,
    Difficulty,
    Variables,
}

#[derive(Clone, Debug)]
pub struct Beatmap<'a> {
    pub context: Context,
    pub general: General<'a>,
    pub colours: Colours<'a>,
    pub editor: Editor,
    pub metadata: Metadata<'a>,
    pub timing_points: Vec<TimingPoint>,
    pub events: Events<'a>,
    pub hit_objects: Vec<HitObject<'a>>,
    pub difficulty: Difficulty,
    pub variables: Variables<'a>,
}

impl<'a> Default for Beatmap<'a> {
    fn default() -> Self {
        Self::default_with_context(Context {
            version: Self::DEFAULT_VERSION,
        })
    }
}

impl<'a> Beatmap<'a> {
    const DEFAULT_VERSION: i32 = 14;

    fn default_with_context(ctx: Context) -> Self {
        Self {
            general: General::default_with_context(&ctx),
            colours: Colours::default_with_context(&ctx),
            editor: Editor::default_with_context(&ctx),
            metadata: Metadata::default_with_context(&ctx),
            timing_points: vec![],
            events: Events::default_with_context(&ctx),
            hit_objects: vec![],
            difficulty: Difficulty::default_with_context(&ctx),
            variables: Variables::default_with_context(&ctx),
            context: ctx,
        }
    }
}

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

impl Beatmap<'static> {
    pub fn parse_file(file: impl io::Read + io::Seek) -> Result<Self, ReadError> {
        let mut file = BufReader::new(file);
        let mut line_buf = String::new();
        let mut pos = 0;
        let mut next_pos = pos + file.read_line(&mut line_buf)?;
        if next_pos == pos {
            return Err(ReadError::Io(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "unexpected end of file",
            )));
        }
        let mut ctx = Context {
            version: Self::DEFAULT_VERSION,
        };
        let version = if line_buf.starts_with("osu file format")
            || line_buf.starts_with("\u{FEFF}osu file format")
        {
            let line = line_buf.trim_end_matches(|x| matches!(x, '\n' | '\r'));
            let line = Lended(line, Span::new(pos, pos + line.len()));
            let version = line.split('v').last().unwrap();
            i32::parse_field("version", &ctx, version)?
        } else {
            println!("defaulting {line_buf}");
            Self::DEFAULT_VERSION
        };
        ctx.version = version;
        let mut ret = Self::default_with_context(ctx);
        let mut section = None::<Section>;
        // Events have to be processed last
        let mut events_pos = None::<u64>;
        loop {
            let line = line_buf.trim_end_matches(|x| matches!(x, '\n' | '\r'));
            let mut skip = line.is_empty() || line.starts_with("//");
            let mut eof = false;
            if !skip
                && section != Some(Section::HitObjects)
                && !line.contains(':')
                && line.starts_with('[')
            {
                if let Ok(x) = line.trim_matches(|x| x == '[' || x == ']').parse() {
                    match (section, x) {
                        (Some(old), new) if old == new => {}
                        (Some(Section::Events), _) => eof = true,
                        (_, Section::Events) => {
                            events_pos = Some(pos as u64);
                            section = None;
                        }
                        (_, x) => section = Some(x),
                    };
                    skip = true;
                }
            }

            if !skip {
                let line = Lended(line, Span::new(pos, pos + line.len()));

                let mut current = section;
                while let Some(section) = current {
                    current = match section {
                        Section::General => ret.general.consume_line(&ret.context, line),
                        Section::Colours => ret.colours.consume_line(&ret.context, line),
                        Section::Editor => ret.editor.consume_line(&ret.context, line),
                        Section::Metadata => ret.metadata.consume_line(&ret.context, line),
                        Section::TimingPoints => ret.timing_points.consume_line(&ret.context, line),
                        Section::Events => {
                            if ret.variables.variables.is_empty() {
                                ret.events.consume_line(&ret.context, line)
                            } else {
                                let mut line = line.as_ref().to_owned();
                                for (var, text) in &ret.variables.variables {
                                    line = line.replace(&("$".to_owned() + var), text);
                                }
                                ret.events.consume_line(
                                    &ret.context,
                                    Lended(&line, Span::new(pos, pos + line.len())),
                                )
                            }
                        }
                        Section::HitObjects => ret.hit_objects.consume_line(&ret.context, line),
                        Section::Difficulty => ret.difficulty.consume_line(&ret.context, line),
                        Section::Variables => ret.variables.consume_line(&ret.context, line),
                    }?;
                }
            }

            line_buf.clear();
            pos = next_pos;
            next_pos = pos + file.read_line(&mut line_buf)?;
            if next_pos == pos {
                eof = true;
            }
            if eof {
                if let Some(pos) = events_pos.take() {
                    file.seek(io::SeekFrom::Start(pos))?;
                    section = Some(Section::Events);
                } else {
                    break;
                }
            }
        }
        Ok(ret)
    }
}

impl<'a> Beatmap<'a> {
    pub fn parse_str(data: &'a str) -> Result<Self, ParseError> {
        let mut pos = 0;
        let mut next_pos = memchr::memchr(b'\n', data.as_bytes())
            .map(|x| x + pos + 1)
            .unwrap_or_else(|| data.len());
        let mut ctx = Context {
            version: Self::DEFAULT_VERSION,
        };
        let version = if data[..next_pos].starts_with("osu file format") {
            let line = data[..next_pos].trim_end_matches(|x| matches!(x, '\n' | '\r'));
            let line = Borrowed(line, Span::new(pos, pos + line.len()));
            let version = line.split('v').last().unwrap();
            i32::parse_field("version", &ctx, version)?
        } else {
            Self::DEFAULT_VERSION
        };
        ctx.version = version;
        let mut ret = Self::default_with_context(ctx);
        let mut section = None::<Section>;
        // Events have to be processed last
        let mut events_pos = None::<(usize, usize)>;
        loop {
            let line = data[pos..next_pos].trim_end_matches(|x| matches!(x, '\n' | '\r'));
            let mut skip = line.is_empty() || line.starts_with("//");
            let mut eof = false;
            if !skip
                && section != Some(Section::HitObjects)
                && !line.contains(':')
                && line.starts_with('[')
            {
                if let Ok(x) = line.trim_matches(|x| x == '[' || x == ']').parse() {
                    match (section, x) {
                        (Some(old), new) if old == new => {}
                        (Some(Section::Events), _) => eof = true,
                        (_, Section::Events) => {
                            events_pos = Some((pos, next_pos));
                            section = None;
                        }
                        (_, x) => section = Some(x),
                    };
                    skip = true;
                }
            }

            if !skip {
                let line = Borrowed(line, Span::new(pos, pos + line.len()));

                let mut current = section;
                while let Some(section) = current {
                    current = match section {
                        Section::General => ret.general.consume_line(&ret.context, line),
                        Section::Colours => ret.colours.consume_line(&ret.context, line),
                        Section::Editor => ret.editor.consume_line(&ret.context, line),
                        Section::Metadata => ret.metadata.consume_line(&ret.context, line),
                        Section::TimingPoints => ret.timing_points.consume_line(&ret.context, line),
                        Section::Events => {
                            if ret.variables.variables.is_empty() {
                                ret.events.consume_line(&ret.context, line)
                            } else {
                                let mut line = line.as_ref().to_owned();
                                for (var, text) in &ret.variables.variables {
                                    line = line.replace(&("$".to_owned() + var), text);
                                }
                                ret.events.consume_line(
                                    &ret.context,
                                    Lended(&line, Span::new(pos, pos + line.len())),
                                )
                            }
                        }
                        Section::HitObjects => ret.hit_objects.consume_line(&ret.context, line),
                        Section::Difficulty => ret.difficulty.consume_line(&ret.context, line),
                        Section::Variables => ret.variables.consume_line(&ret.context, line),
                    }?;
                }
            }

            pos = next_pos;
            next_pos = memchr::memchr(b'\n', data[pos..].as_bytes())
                .map(|x| x + pos + 1)
                .unwrap_or_else(|| data.len());
            if next_pos == pos {
                eof = true;
            }
            if eof {
                if let Some((pos1, next_pos1)) = events_pos.take() {
                    pos = pos1;
                    next_pos = next_pos1;
                    section = Some(Section::Events);
                } else {
                    break;
                }
            }
        }
        Ok(ret)
    }
    /// Version-specific behavior is preserved on a best-effort basis (there isn't a particular set
    /// of rules that could be followed in general)
    pub fn serialize(&self, out: impl io::Write) -> io::Result<()> {
        let mut out = io::BufWriter::new(out);
        write!(
            out,
            "osu file format v{}\r\n\
            \r\n\
            [General]\r\n\
            AudioFilename: {}\r\n",
            self.context.version, self.general.audio_filename
        )?;
        if self.context.version > 3 || self.general.audio_lead_in != 0 {
            write!(out, "AudioLeadIn: {}\r\n", self.general.audio_lead_in)?;
        }
        if !self.general.audio_hash.is_empty() {
            write!(out, "AudioHash: {}\r\n", self.general.audio_hash)?;
        }
        write!(out, "PreviewTime: {}\r\n", self.general.preview_time)?;
        if self.context.version > 3 || !matches!(self.general.countdown, Countdown::Normal) {
            write!(out, "Countdown: {}\r\n", self.general.countdown as i32)?;
        }
        write!(out, "SampleSet: {}\r\n", self.general.sample_set)?;
        // actually added mid-version 5
        if self.context.version > 4 {
            write!(
                out,
                "StackLeniency: {}\r\n\
                Mode: {}\r\n\
                LetterboxInBreaks: {}\r\n",
                self.general.stack_leniency,
                self.general.mode as i32,
                u8::from(self.general.letterbox_in_breaks),
            )?;
        }

        if !self.general.story_fire_in_front {
            out.write_all(b"StoryFireInFront: 0\r\n")?;
        }
        if self.general.use_skin_sprites {
            out.write_all(b"UseSkinSprites: 1\r\n")?;
        }
        if self.general.always_show_playfield {
            out.write_all(b"AlwaysShowPlayfield: 1\r\n")?;
        }
        if self.general.overlay_position != OverlayPosition::NoChange {
            write!(
                out,
                "OverlayPosition: {}\r\n",
                self.general.overlay_position as i32
            )?;
        }
        if !self.general.skin_preference.is_empty() {
            write!(out, "SkinPreference:{}\r\n", self.general.skin_preference)?;
        }
        if self.general.epilepsy_warning {
            out.write_all(b"EpilepsyWarning: 1\r\n")?;
        }
        if self.general.countdown_offset > 0 {
            write!(
                out,
                "CountdownOffset: {}\r\n",
                self.general.countdown_offset
            )?;
        }
        if self.general.mode == GameMode::Mania {
            write!(
                out,
                "SpecialStyle: {}\r\n",
                u8::from(self.general.special_style)
            )?;
        }
        if self.context.version > 11 || self.general.widescreen_storyboard {
            write!(
                out,
                "WidescreenStoryboard: {}\r\n",
                u8::from(self.general.widescreen_storyboard)
            )?;
        }
        if self.general.samples_match_playback_rate {
            out.write_all(b"SamplesMatchPlaybackRate: 1\r\n")?;
        }
        if self.context.version > 5 {
            out.write_all(b"\r\n[Editor]\r\n")?;

            if !self.editor.bookmarks.is_empty() {
                out.write_all(b"Bookmarks: ")?;
                let mut first = true;
                for bookmark in &self.editor.bookmarks {
                    if first {
                        first = false;
                    } else {
                        out.write_all(b",")?;
                    }
                    write!(out, "{bookmark}")?;
                }
                out.write_all(b"\r\n")?;
            }

            write!(
                out,
                "DistanceSpacing: {}\r\n\
                BeatDivisor: {}\r\n\
                GridSize: {}\r\n",
                self.editor.distance_spacing, self.editor.beat_divisor, self.editor.grid_size,
            )?;
            if self.context.version > 12 || self.editor.timeline_zoom != 1.0 {
                write!(out, "TimelineZoom: {}\r\n", self.editor.timeline_zoom)?;
            }
            write!(out, "\r\n")?;
        } else {
            if !self.editor.bookmarks.is_empty() {
                out.write_all(b"EditorBookmarks: ")?;
                let mut first = true;
                for bookmark in &self.editor.bookmarks {
                    if first {
                        first = false;
                    } else {
                        out.write_all(b",")?;
                    }
                    write!(out, "{bookmark}")?;
                }
                out.write_all(b"\r\n")?;
            }
            if self.editor.distance_spacing != 0.8 {
                write!(
                    out,
                    "EditorDistanceSpacing: {}\r\n",
                    self.editor.distance_spacing
                )?;
            }
            write!(out, "\r\n")?;
        }
        write!(
            out,
            "[Metadata]\r\n\
            Title:{}\r\n",
            self.metadata.title
        )?;
        if !self.metadata.title_unicode.is_empty() || self.context.version > 9 {
            write!(out, "TitleUnicode:{}\r\n", self.metadata.title_unicode)?;
        }
        write!(out, "Artist:{}\r\n", self.metadata.artist)?;
        if !self.metadata.artist_unicode.is_empty() || self.context.version > 9 {
            write!(out, "ArtistUnicode:{}\r\n", self.metadata.artist_unicode)?;
        }
        write!(out, "Creator:{}\r\n", self.metadata.creator)?;
        write!(out, "Version:{}\r\n", self.metadata.version)?;
        // actually added mid-version 5
        if !self.metadata.source.is_empty() || self.context.version > 4 {
            write!(out, "Source:{}\r\n", self.metadata.source)?;
        }
        // actually added mid-version 5
        if !self.metadata.tags.is_empty() || self.context.version > 4 {
            write!(out, "Tags:{}\r\n", self.metadata.tags)?;
        }
        if self.metadata.beatmap_id != 0 || self.context.version > 9 {
            write!(out, "BeatmapID:{}\r\n", self.metadata.beatmap_id)?;
        }
        if self.metadata.beatmap_set_id != -1 || self.context.version > 9 {
            write!(out, "BeatmapSetID:{}\r\n", self.metadata.beatmap_set_id)?;
        }
        write!(
            out,
            "\r\n\
            [Difficulty]\r\n\
            HPDrainRate:{}\r\n\
            CircleSize:{}\r\n\
            OverallDifficulty:{}\r\n",
            self.difficulty.hp_drain_rate,
            self.difficulty.circle_size,
            self.difficulty.overall_difficulty,
        )?;
        if self.difficulty.approach_rate != 5.0 || self.context.version > 7 {
            write!(out, "ApproachRate:{}\r\n", self.difficulty.approach_rate)?;
        }
        write!(
            out,
            "SliderMultiplier:{0}{1}\r\n\
            SliderTickRate:{0}{2}\r\n\
            \r\n\
            [Events]\r\n",
            if self.context.version > 3 { "" } else { " " },
            self.difficulty.slider_multiplier,
            self.difficulty.slider_tick_rate
        )?;
        if self.context.version > 3 {
            write!(out, "//Background and Video events\r\n")?;
        }

        for event in self.events.events.iter().filter(|x| {
            matches!(
                x.object,
                EventObject::Background { .. } | EventObject::Video { .. }
            )
        }) {
            match &event.object {
                EventObject::Background {
                    filename,
                    time,
                    x,
                    y,
                } => {
                    if self.context.version > 11 || *x != 0 || *y != 0 {
                        write!(
                            out,
                            "0,{},\"{filename}\",{x},{y}\r\n",
                            time.serialize(self.context.version)
                        )?;
                    } else {
                        write!(
                            out,
                            "0,{},\"{filename}\"\r\n",
                            time.serialize(self.context.version)
                        )?;
                    }
                }
                EventObject::Video {
                    filename,
                    time,
                    x,
                    y,
                } => {
                    let tag = if self.context.version > 5 {
                        "Video"
                    } else {
                        "1"
                    };
                    if *x == 0 && *y == 0 {
                        write!(
                            out,
                            "{tag},{},\"{filename}\"\r\n",
                            time.serialize(self.context.version)
                        )?;
                    } else {
                        write!(
                            out,
                            "{tag},{},\"{filename}\",{x},{y}\r\n",
                            time.serialize(self.context.version)
                        )?;
                    }
                }
                _ => unreachable!(),
            }
        }
        if self.context.version > 3 {
            write!(out, "//Break Periods\r\n")?;
        }
        for event in self
            .events
            .events
            .iter()
            .filter(|x| matches!(x.object, EventObject::Break { .. }))
        {
            match &event.object {
                EventObject::Break { time, end_time } => {
                    write!(
                        out,
                        "2,{},{}\r\n",
                        time.serialize(self.context.version),
                        end_time.serialize(self.context.version)
                    )?;
                }
                _ => unreachable!(),
            }
        }
        for layer in [
            EventLayer::Background,
            EventLayer::Fail,
            EventLayer::Pass,
            EventLayer::Foreground,
            EventLayer::Overlay,
        ] {
            if self.context.version > 3 {
                out.write_all(match layer {
                    EventLayer::Background => b"//Storyboard Layer 0 (Background)\r\n",
                    // actually changed mid-version 5
                    EventLayer::Fail if self.context.version > 5 => {
                        b"//Storyboard Layer 1 (Fail)\r\n"
                    }
                    EventLayer::Pass if self.context.version > 5 => {
                        b"//Storyboard Layer 2 (Pass)\r\n"
                    }
                    EventLayer::Fail => b"//Storyboard Layer 1 (Failing)\r\n",
                    EventLayer::Pass => b"//Storyboard Layer 2 (Passing)\r\n",
                    EventLayer::Foreground => b"//Storyboard Layer 3 (Foreground)\r\n",
                    EventLayer::Overlay if self.context.version > 12 => {
                        b"//Storyboard Layer 4 (Overlay)\r\n"
                    }
                    EventLayer::Overlay => b"",
                })?;
            }
            for event in self.events.events.iter().filter(|x| {
                matches!(
                    x.object,
                    EventObject::Animation { layer: layer1, .. } | EventObject::Sprite { layer: layer1, .. } if layer == layer1
                )
            }) {
                match &event.object {
                    EventObject::Sprite {
                        filename,
                        x,
                        y,
                        origin,
                        layer,
                    } => {
                        write!(
                            out,
                            "4,{},{},\"{}\",{},{}\r\n",
                            *layer as i32, *origin as i32, filename, x, y,
                        )?;
                    }
                    EventObject::Animation {
                        filename,
                        x,
                        y,
                        origin,
                        layer,
                        frame_count,
                        frame_delay,
                        loop_type,
                    } => {
                        write!(
                            out,
                            "6,{},{},\"{}\",{},{},{},{}",
                            *layer as i32,
                            *origin as i32,
                            filename,
                            x,
                            y,
                            frame_count,
                            frame_delay,
                        )?;
                        if self.context.version > 5 || !matches!(loop_type, EventLoop::LoopForever) {
                            write!(out, ",{}", *loop_type as i32)?;
                        }
                        write!(out, "\r\n")?;
                    }
                    _ => unreachable!(),
                }
                for cmd in &event.commands {
                    match cmd {
                        EventCommandSequence::Basic(x) => {
                            write!(out, " ")?;
                            x.write(&self.context, &mut out)?;
                            write!(out, "\r\n")?;
                        }
                        EventCommandSequence::Loop(x, cmds) => {
                            write!(out, " L")?;
                            for x in x {
                                write!(out, ",{},{}", x.time.serialize(self.context.version), x.count)?;
                            }
                            write!(out, "\r\n")?;
                            for cmd in cmds {
                                write!(out, "  ")?;
                                cmd.write(&self.context, &mut out)?;
                                write!(out, "\r\n")?;
                            }
                        }
                        EventCommandSequence::Trigger(x, cmds) => {
                            write!(out, " T")?;
                            for x in x {
                                write!(out, ",{}", x.trigger)?;
                                if let Some((start, end)) = x.time_range {
                                    write!(out, ",{},{}", start.serialize(self.context.version), end.serialize(self.context.version))?;
                                    if let Some(group) = x.trigger_group {
                                        write!(out, ",{group}")?;
                                    }
                                }
                            }
                            write!(out, "\r\n")?;
                            for cmd in cmds {
                                write!(out, "  ")?;
                                cmd.write(&self.context, &mut out)?;
                                write!(out, "\r\n")?;
                            }
                        }
                    }
                }
            }
        }
        if self.context.version > 3 {
            write!(out, "//Storyboard Sound Samples\r\n")?;
        }
        for event in self
            .events
            .events
            .iter()
            .filter(|x| matches!(x.object, EventObject::Sample { .. }))
        {
            match &event.object {
                EventObject::Sample {
                    filename,
                    time,
                    volume,
                    layer,
                } => {
                    write!(
                        out,
                        "5,{},{},\"{}\"",
                        time.serialize(self.context.version),
                        *layer as i32,
                        filename
                    )?;
                    if self.context.version > 5 || *volume != 100. {
                        write!(out, ",{volume}")?;
                    }
                    write!(out, "\r\n")?;
                }
                _ => unreachable!(),
            }
        }
        if self
            .events
            .events
            .iter()
            .any(|x| matches!(x.object, EventObject::Colour { .. }))
        {
            if self.context.version > 3 {
                write!(out, "//Background Colour Transformations\r\n")?;
            }
            for event in self
                .events
                .events
                .iter()
                .filter(|x| matches!(x.object, EventObject::Colour { .. }))
            {
                match &event.object {
                    EventObject::Colour { time, colour } => {
                        write!(
                            out,
                            "3,{},{},{},{}\r\n",
                            time.serialize(self.context.version),
                            colour.0,
                            colour.1,
                            colour.2
                        )?;
                    }
                    _ => unreachable!(),
                }
            }
        }
        write!(out, "\r\n")?;

        if !self.timing_points.is_empty() {
            write!(out, "[TimingPoints]\r\n")?;
            for timing_point in &self.timing_points {
                if timing_point.beat_length != 0.0 {
                    write!(
                        out,
                        "{},{}",
                        timing_point.offset.serialize(self.context.version),
                        timing_point.beat_length
                    )?;
                    if self.context.version > 3
                        || !timing_point.changes_timing
                        || !timing_point.flags.is_empty()
                        || timing_point.time_signature != 4
                        || timing_point
                            .sample_set
                            .filter(|x| *x != self.general.sample_set)
                            .is_some()
                        || timing_point.custom_sample_index != 0
                        || timing_point.sample_volume != 100
                    {
                        write!(
                            out,
                            ",{},{},{},{}",
                            timing_point.time_signature,
                            timing_point.sample_set.unwrap_or(self.general.sample_set) as i32,
                            timing_point.custom_sample_index,
                            timing_point.sample_volume
                        )?;
                    }
                    // actually added mid-version 5 (first one, then the other)
                    if self.context.version > 4
                        || !timing_point.changes_timing
                        || !timing_point.flags.is_empty()
                    {
                        write!(
                            out,
                            ",{},{}\r\n",
                            u8::from(timing_point.changes_timing),
                            timing_point.flags.bits()
                        )?;
                    } else {
                        write!(out, "\r\n")?;
                    }
                }
            }
            write!(out, "\r\n")?;
        }

        if self.context.version > 8 {
            write!(out, "\r\n")?;
        }

        if !self.colours.colours.is_empty() {
            write!(out, "[Colours]\r\n")?;
            for (k, (r, g, b)) in &self.colours.colours {
                write!(out, "{k} : {r},{g},{b}\r\n")?;
            }
            write!(out, "\r\n")?;
        }

        write!(out, "[HitObjects]\r\n")?;

        for h in &self.hit_objects {
            write!(
                out,
                "{},{},{},{},{}",
                h.x,
                h.y,
                h.time,
                h.flags().bits(),
                h.hit_sound.hit_sound.sounds.bits()
            )?;
            let extra_sep = match &h.kind {
                HitObjectKind::Circle => Some(','),
                HitObjectKind::Slider {
                    kind,
                    curve_points,
                    length,
                    edge_sounds,
                    slide_count,
                } => {
                    write!(out, ",{}", char::from(*kind))?;
                    for (x, y) in curve_points {
                        write!(out, "|{x}:{y}")?;
                    }
                    if edge_sounds.is_empty() {
                        write!(out, ",{slide_count},{length}")?;
                        None
                    } else {
                        write!(out, ",{slide_count},{length},")?;
                        let mut first = true;
                        for sound in edge_sounds {
                            if first {
                                write!(out, "{}", sound.sounds.bits())?;
                                first = false;
                            } else {
                                write!(out, "|{}", sound.sounds.bits())?;
                            }
                        }
                        if self.context.version > 9
                            || edge_sounds.iter().any(|x| {
                                !matches!(x.sample_set, SampleSet::None)
                                    || !matches!(x.addition_set, SampleSet::None)
                            })
                        {
                            write!(out, ",")?;
                            first = true;
                            for sound in edge_sounds {
                                if first {
                                    write!(
                                        out,
                                        "{}:{}",
                                        sound.sample_set as i32, sound.addition_set as i32
                                    )?;
                                    first = false;
                                } else {
                                    write!(
                                        out,
                                        "|{}:{}",
                                        sound.sample_set as i32, sound.addition_set as i32
                                    )?;
                                }
                            }
                        }
                        Some(',')
                    }
                }
                HitObjectKind::Spinner { end_time } => {
                    write!(out, ",{end_time}")?;
                    Some(',')
                }
                HitObjectKind::HoldNote { end_time } => {
                    write!(out, ",{end_time}")?;
                    Some(':')
                }
            };
            if let Some(ch) = extra_sep.filter(|_| {
                if self.context.version > 9 {
                    true
                } else {
                    !h.hit_sound.sample_file.is_empty()
                        || !matches!(h.hit_sound.hit_sound.sample_set, SampleSet::None)
                        || !matches!(h.hit_sound.hit_sound.addition_set, SampleSet::None)
                        || h.hit_sound.custom_sample_index != 0
                        || h.hit_sound.volume != 0
                }
            }) {
                if self.context.version > 11
                    || !h.hit_sound.sample_file.is_empty()
                    || h.hit_sound.volume != 0
                {
                    write!(
                        out,
                        "{ch}{}:{}:{}:{}:{}\r\n",
                        h.hit_sound.hit_sound.sample_set as i32,
                        h.hit_sound.hit_sound.addition_set as i32,
                        h.hit_sound.custom_sample_index,
                        h.hit_sound.volume,
                        h.hit_sound.sample_file
                    )?;
                } else {
                    write!(
                        out,
                        "{ch}{}:{}:{}\r\n",
                        h.hit_sound.hit_sound.sample_set as i32,
                        h.hit_sound.hit_sound.addition_set as i32,
                        h.hit_sound.custom_sample_index,
                    )?;
                }
            } else if self.context.version < 4 && matches!(h.kind, HitObjectKind::Circle) {
                write!(out, ",\r\n")?;
            } else {
                write!(out, "\r\n")?;
            }
        }
        Ok(())
    }
}
