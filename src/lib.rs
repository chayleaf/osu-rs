#![allow(clippy::unit_arg)]
use bitflags::bitflags;
use osu_rs_derive::{BeatmapEnum, BeatmapSection};
use parsers::{InvalidColour, InvalidEvent, ParseError};
use std::{
    borrow::Cow,
    io::{self, BufRead, BufReader, Seek, Write},
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

/// Game modes
#[derive(BeatmapEnum, Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum GameMode {
    Osu = 0,
    Taiko = 1,
    CatchTheBeat = 2,
    Mania = 3,
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
    pub slider_multiplier: f32,
    pub slider_tick_rate: f32,
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
            let g = ParseField::parse_field("red colour channel", ctx, g)?;
            let b = ParseField::parse_field("red colour channel", ctx, b)?;
            self.colours.push((key.into_a_cow(), (r, g, b)));
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
            self.variables.push((key.into_a_cow(), value.into_a_cow()));
        }
        Ok(None)
    }
}

#[derive(BeatmapEnum, Clone, Copy, PartialEq, Eq, Hash)]
enum EventKind {
    Background = 0,
    Video = 1,
    Break = 2,
    Colour = 3,
    Sprite = 4,
    Sample = 5,
    Animation = 6,
}

#[derive(Clone, Debug)]
pub enum Event<'a> {
    Background {
        filename: Cow<'a, str>,
        time: i32,
        x: i32,
        y: i32,
    },
    Video {
        filename: Cow<'a, str>,
        time: i32,
        x: i32,
        y: i32,
    },
    Break {
        time: i32,
        end_time: i32,
    },
    Colour {
        time: i32,
        colour: Colour,
    },
    /*Sprite {
        filename: Cow<'a, str>,
        x: f64,
        y: f64,
        origin: EventOrigin,
        layer: EventLayer,
    },
    Sample {
        filename: Cow<'a, str>,
        time: i32,
        volume: f64,
        layer: EventLayer,
    },
    Animation {
        frame_count: i32,
        frame_delay: f64,
        loop_type: EventLoop,
    },*/
}

/// Events section
#[derive(Clone, Debug)]
pub struct Events<'a> {
    pub events: Vec<Event<'a>>,
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
        let (kind, s) = line
            .split_once(',')
            .ok_or(InvalidEvent)
            .map_err(ParseError::curry("event", line.span()))?;
        let mut end_span = line.span();
        end_span.start = end_span.end;
        let event = match EventKind::parse_field("event type", ctx, kind)? {
            EventKind::Background => {
                let (time, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("background event filename", end_span))?;
                let time = ParseField::parse_field("background event time", ctx, time)?;
                let (filename, s) = s
                    .split_once(',')
                    .map(|(a, b)| (a, Some(b)))
                    .unwrap_or((s, None));
                let filename = filename.trim_matches('"').into_a_cow();
                let (x, y) = if let Some((x, y)) = s.and_then(|x| x.split_once(',')) {
                    let y = y.split_once(',').map(|x| x.0).unwrap_or(y);
                    let x = ParseField::parse_field("background event x", ctx, x)?;
                    let y = ParseField::parse_field("background event x", ctx, y)?;
                    (x, y)
                } else {
                    (0, 0)
                };
                Some(Event::Background {
                    filename,
                    time,
                    x,
                    y,
                })
            }
            EventKind::Video => {
                let (time, s) = s
                    .split_once(',')
                    .ok_or(InvalidEvent)
                    .map_err(ParseError::curry("video event filename", end_span))?;
                let time = ParseField::parse_field("video event time", ctx, time)?;
                let (filename, s) = s
                    .split_once(',')
                    .map(|(a, b)| (a, Some(b)))
                    .unwrap_or((s, None));
                let filename = filename.trim_matches('"').into_a_cow();
                let (x, y) = if let Some((x, y)) = s.and_then(|x| x.split_once(',')) {
                    let y = y.split_once(',').map(|x| x.0).unwrap_or(y);
                    let x = ParseField::parse_field("video event x", ctx, x)?;
                    let y = ParseField::parse_field("video event x", ctx, y)?;
                    (x, y)
                } else {
                    (0, 0)
                };
                Some(Event::Video {
                    filename,
                    time,
                    x,
                    y,
                })
            }
            EventKind::Break => {
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
                Some(Event::Break { time, end_time })
            }
            // TODO
            _ => None,
        };
        if let Some(event) = event {
            self.events.push(event);
        }
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
    pub offset: f64,
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
        let version = if line_buf.starts_with("osu file format") {
            let line = line_buf.trim_end_matches(|x| matches!(x, '\n' | '\r'));
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
                        Section::Events => ret.events.consume_line(&ret.context, line),
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
    pub fn parse_str(data: &str) -> Result<Self, ParseError> {
        let mut line_buf = String::new();
        let mut pos = 0;
        let mut next_pos = memchr::memchr(b'\n', data.as_bytes())
            .map(|x| x + pos + 1)
            .unwrap_or_else(|| data.len());
        let mut ctx = Context {
            version: Self::DEFAULT_VERSION,
        };
        let version = if line_buf.starts_with("osu file format") {
            let line = line_buf.trim_end_matches(|x| matches!(x, '\n' | '\r'));
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
                let line = Lended(line, Span::new(pos, pos + line.len()));

                let mut current = section;
                while let Some(section) = current {
                    current = match section {
                        Section::General => ret.general.consume_line(&ret.context, line),
                        Section::Colours => ret.colours.consume_line(&ret.context, line),
                        Section::Editor => ret.editor.consume_line(&ret.context, line),
                        Section::Metadata => ret.metadata.consume_line(&ret.context, line),
                        Section::TimingPoints => ret.timing_points.consume_line(&ret.context, line),
                        Section::Events => ret.events.consume_line(&ret.context, line),
                        Section::HitObjects => ret.hit_objects.consume_line(&ret.context, line),
                        Section::Difficulty => ret.difficulty.consume_line(&ret.context, line),
                        Section::Variables => ret.variables.consume_line(&ret.context, line),
                    }?;
                }
            }

            line_buf.clear();
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
    pub fn serialize(&self, out: impl io::Write) -> io::Result<()> {
        assert_eq!(self.context.version, Self::DEFAULT_VERSION);
        let mut out = io::BufWriter::new(out);
        write!(
            out,
            "osu file format v{}\r\n\
        \r\n\
        [General]\r\n\
        AudioFilename: {}\r\n\
        AudioLeadIn: {}\r\n\
        PreviewTime: {}\r\n\
        Countdown: {}\r\n\
        SampleSet: {}\r\n\
        StackLeniency: {}\r\n\
        Mode: {}\r\n\
        LetterboxInBreaks: {}\r\n",
            self.context.version,
            self.general.audio_filename,
            self.general.audio_lead_in,
            self.general.preview_time,
            self.general.countdown as i32,
            self.general.sample_set,
            self.general.stack_leniency,
            self.general.mode as i32,
            u8::from(self.general.letterbox_in_breaks),
        )?;

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
            write!(out, "SkinPreference: {}\r\n", self.general.skin_preference)?;
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
        write!(
            out,
            "WidescreenStoryboard: {}\r\n",
            u8::from(self.general.widescreen_storyboard)
        )?;
        if self.general.samples_match_playback_rate {
            out.write_all(b"SamplesMatchPlaybackRate: 1\r\n")?;
        }
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
        GridSize: {}\r\n\
        TimelineZoom: {}\r\n\
        \r\n\
        [Metadata]\r\n\
        Title:{}\r\n\
        TitleUnicode:{}\r\n\
        Artist:{}\r\n\
        ArtistUnicode:{}\r\n\
        Creator:{}\r\n\
        Version:{}\r\n\
        Source:{}\r\n\
        Tags:{}\r\n\
        BeatmapID:{}\r\n\
        BeatmapSetID:{}\r\n\
        \r\n\
        [Difficulty]\r\n\
        HPDrainRate:{}\r\n\
        CircleSize:{}\r\n\
        OverallDifficulty:{}\r\n\
        ApproachRate:{}\r\n\
        SliderMultiplier:{}\r\n\
        SliderTickRate:{}\r\n\
        \r\n\
        [Events]\r\n\
        //Background and Video events\r\n",
            self.editor.distance_spacing,
            self.editor.beat_divisor,
            self.editor.grid_size,
            self.editor.timeline_zoom,
            self.metadata.title,
            self.metadata.title_unicode,
            self.metadata.artist,
            self.metadata.artist_unicode,
            self.metadata.creator,
            self.metadata.version,
            self.metadata.source,
            self.metadata.tags,
            self.metadata.beatmap_id,
            self.metadata.beatmap_set_id,
            self.difficulty.hp_drain_rate,
            self.difficulty.circle_size,
            self.difficulty.overall_difficulty,
            self.difficulty.approach_rate,
            self.difficulty.slider_multiplier,
            self.difficulty.slider_tick_rate
        )?;

        for event in self
            .events
            .events
            .iter()
            .filter(|x| matches!(x, Event::Background { .. } | Event::Video { .. }))
        {
            match event {
                Event::Background {
                    filename,
                    time,
                    x,
                    y,
                } => {
                    write!(out, "0,{time},\"{filename}\",{x},{y}\r\n")?;
                }
                Event::Video {
                    filename,
                    time,
                    x,
                    y,
                } => {
                    if *x == 0 && *y == 0 {
                        write!(out, "Video,{time},\"{filename}\"\r\n")?;
                    } else {
                        write!(out, "Video,{time},\"{filename}\",{x},{y}\r\n")?;
                    }
                }
                _ => unreachable!(),
            }
        }
        write!(out, "//Break Periods\r\n")?;
        for event in self
            .events
            .events
            .iter()
            .filter(|x| matches!(x, Event::Break { .. }))
        {
            match event {
                Event::Break { time, end_time } => {
                    write!(out, "2,{time},{end_time}\r\n")?;
                }
                _ => unreachable!(),
            }
        }
        write!(
            out,
            "//Storyboard Layer 0 (Background)\r\n\
        //Storyboard Layer 1 (Fail)\r\n\
        //Storyboard Layer 2 (Pass)\r\n\
        //Storyboard Layer 3 (Foreground)\r\n\
        //Storyboard Layer 4 (Overlay)\r\n\
        //Storyboard Sound Samples\r\n\r\n"
        )?;

        if !self.timing_points.is_empty() {
            write!(out, "[TimingPoints]\r\n")?;
            for timing_point in &self.timing_points {
                if timing_point.beat_length != 0.0 {
                    write!(
                        out,
                        "{},{},{},{},{},{},{},{}\r\n",
                        timing_point.offset,
                        timing_point.beat_length,
                        timing_point.time_signature,
                        timing_point.sample_set.unwrap_or(self.general.sample_set) as i32,
                        timing_point.custom_sample_index,
                        timing_point.sample_volume,
                        u8::from(timing_point.changes_timing),
                        timing_point.flags.bits()
                    )?;
                }
            }
            write!(out, "\r\n")?;
        }

        if !self.colours.colours.is_empty() {
            write!(out, "\r\n[Colours]\r\n")?;
            for (k, (r, g, b)) in &self.colours.colours {
                write!(out, "{k} : {r},{g},{b}\r\n")?;
            }
        }

        write!(out, "\r\n[HitObjects]\r\n")?;

        for h in &self.hit_objects {
            write!(
                out,
                "{},{},{},{},{},",
                h.x,
                h.y,
                h.time,
                h.flags().bits(),
                h.hit_sound.hit_sound.sounds.bits()
            )?;
            let write_extra = match &h.kind {
                HitObjectKind::Circle => true,
                HitObjectKind::Slider {
                    kind,
                    curve_points,
                    length,
                    edge_sounds,
                    slide_count,
                } => {
                    write!(out, "{}", char::from(*kind))?;
                    for (x, y) in curve_points {
                        write!(out, "|{x}:{y}")?;
                    }
                    if edge_sounds.is_empty() {
                        write!(out, ",{slide_count},{length}\r\n")?;
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
                        write!(out, ",")?;
                    }
                    !edge_sounds.is_empty()
                }
                HitObjectKind::Spinner { end_time } => {
                    write!(out, "{end_time},")?;
                    true
                }
                HitObjectKind::HoldNote { end_time } => {
                    write!(out, "{end_time}:")?;
                    true
                }
            };
            if write_extra {
                write!(
                    out,
                    "{}:{}:{}:{}:{}\r\n",
                    h.hit_sound.hit_sound.sample_set as i32,
                    h.hit_sound.hit_sound.addition_set as i32,
                    h.hit_sound.custom_sample_index,
                    h.hit_sound.volume,
                    h.hit_sound.sample_file
                )?;
            }
        }
        Ok(())
    }
}
