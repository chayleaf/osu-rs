//! skin.ini parsing and serialization.

use std::{
    borrow::Cow,
    io::{self, BufRead, BufReader, Seek, Write},
};

use osu_rs_derive::{SkinEnum, SkinSection};

use crate::{
    beatmap::ReadError,
    error::{
        EnumParseError, IntEnumParseError, MissingKeycountError, ParseError, RecordParseError,
        Span, UnknownSectionName,
    },
    parsers::skin::{ParseField, ParseFieldInPlace, SerializeBox, SerializeField},
    util::{Borrowed, Lended, StaticCow},
};

pub type ColourRgb = (u8, u8, u8);
pub type ColourRgba = (u8, u8, u8, u8);

pub enum ParseStrictness {
    Strict,
    IgnoreUnknownKeys,
    IgnoreErrors,
}

pub struct DeserializationContext {
    pub strictness: ParseStrictness,
}

trait SkinSection<'a> {
    fn consume_line(
        &mut self,
        ctx: &DeserializationContext,
        key: impl StaticCow<'a>,
        value: impl StaticCow<'a>,
    ) -> Result<(), ParseError>;
}

#[derive(SkinEnum, Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SliderStyle {
    PeppySliders = 1,
    MmSliders = 2,
    ToonSliders = 3,
    OpenGlSliders = 4,
}

#[derive(SkinSection, Clone, Debug, PartialEq)]
pub struct General<'a> {
    #[default(Cow::Borrowed("Unknown"))]
    pub name: Cow<'a, str>,
    #[default(Cow::Borrowed(""))]
    pub author: Cow<'a, str>,
    #[default(false)]
    pub slider_ball_flip: bool,
    #[default(true)]
    pub cursor_rotate: bool,
    #[default(true)]
    pub cursor_expand: bool,
    #[default(true)]
    pub cursor_centre: bool,
    #[default(10)]
    pub slider_ball_frames: i32,
    #[default(true)]
    #[alias("HitCircleOverlayAboveNumer")]
    pub hit_circle_overlay_above_number: bool,
    #[default(true)]
    pub spinner_frequency_modulate: bool,
    #[default(true)]
    pub layered_hit_sounds: bool,
    #[default(false)]
    pub spinner_fade_playfield: bool,
    #[default(false)]
    pub spinner_no_blink: bool,
    #[default(false)]
    pub allow_slider_ball_tint: bool,
    #[default(-1)]
    pub animation_framerate: i32,
    #[default(false)]
    pub cursor_trail_rotate: bool,
    #[default(Vec::new())]
    pub custom_combo_burst_sounds: Vec<i32>,
    #[default(false)]
    pub combo_burst_random: bool,
    #[default(SliderStyle::MmSliders)]
    pub slider_style: SliderStyle,
    /// Latest version if None
    #[version]
    pub version: Option<f64>,
}

#[derive(SkinSection, Clone, Debug, PartialEq)]
pub struct Colours {
    #[default((255, 192, 0))]
    pub combo1: ColourRgb,
    #[default((0, 202, 0))]
    pub combo2: ColourRgb,
    #[default((18, 124, 255))]
    pub combo3: ColourRgb,
    #[default((242, 24, 57))]
    pub combo4: ColourRgb,
    #[default(None)]
    pub combo5: Option<ColourRgb>,
    #[default(None)]
    pub combo6: Option<ColourRgb>,
    #[default(None)]
    pub combo7: Option<ColourRgb>,
    #[default(None)]
    pub combo8: Option<ColourRgb>,
    #[default((0, 78, 155))]
    pub menu_glow: ColourRgb,
    #[default((2, 170, 255))]
    pub slider_ball: ColourRgb,
    #[default((255, 255, 255))]
    pub slider_border: ColourRgb,
    #[default((77, 139, 217))]
    pub spinner_approach_circle: ColourRgb,
    #[default((0, 0, 0))]
    pub song_select_active_text: ColourRgb,
    #[default((255, 255, 255))]
    pub song_select_inactive_text: ColourRgb,
    #[default((255, 182, 193))]
    pub star_break_additive: ColourRgb,
    #[default((0, 0, 0))]
    pub input_overlay_text: ColourRgb,
    /// Also applies to taiko for some reason
    #[default(None)]
    pub slider_track_override: Option<ColourRgb>,
    #[default((100, 100, 100))]
    pub spinner_background: ColourRgb,
    #[number_with_prefix("Triangle")]
    pub triangles: Vec<ColourRgb>,
}

#[derive(SkinSection, Clone, Debug, PartialEq)]
pub struct Fonts<'a> {
    #[default(Cow::Borrowed("default"))]
    pub hit_circle_prefix: Cow<'a, str>,
    #[default(0)]
    pub hit_circle_overlap: i32,
    #[default(Cow::Borrowed("score"))]
    pub score_prefix: Cow<'a, str>,
    #[default(0)]
    pub score_overlap: i32,
    #[default(Cow::Borrowed("score"))]
    pub combo_prefix: Cow<'a, str>,
    #[default(0)]
    pub combo_overlap: i32,
}

#[derive(SkinSection, Clone, Debug, PartialEq)]
pub struct CatchTheBeat {
    #[default((255, 0, 0, 255))]
    pub hyper_dash: ColourRgba,
    /// Defaults to `hyper_dash`'s value
    #[default(None)]
    pub hyper_dash_after_image: Option<ColourRgba>,
    /// Defaults to `hyper_dash`'s value
    #[default(None)]
    pub hyper_dash_fruit: Option<ColourRgba>,
}

#[derive(SkinEnum, Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ManiaSpecialStyle {
    None = 0,
    Left = 1,
    Right = 2,
}

#[derive(SkinEnum, Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ManiaNoteBodyStyle {
    Stretch = 0,
    Repeat = 1,
    RepeatTop = 2,
    RepeatBottom = 3,
    RepeatTopAndBottom = 4,
    RepeatMiddle = 5,
}

// mania/mania2 in derive macro means the Box<[T]> length is dependent on keycount
// mania2 means its all in a single line (1,2,3,4)
// mania means its a prefix/suffix with 1-based indexing
// (if prefix = a, suffix = b, then a1b is the first element)
// the last argument to mania/mania2 specifies the default values
#[derive(SkinSection, Clone, Debug, PartialEq)]
pub struct Mania<'a> {
    /// From 0 to 18 (inclusive).
    #[skip]
    pub keys: u8,
    /// Note that everything must be <= 0 or >= 2 (otherwise it gets replaced with 2)
    #[mania2(2.0f32)]
    pub column_line_width: Box<[f32]>,
    #[default(1.2f32)]
    pub barline_height: f32,
    #[default(ManiaSpecialStyle::None)]
    pub special_style: ManiaSpecialStyle,
    /// Clamped between 5 and 100
    #[mania2(30.0f32)]
    pub column_width: Box<[f32]>,
    // `column_spacing[i] >= -column_width[i + 1]` must be upheld for all `i` under `keys - 1`
    #[mania2(0.0f32)]
    pub column_spacing: Box<[f32]>,
    #[mania2(0.0f32)]
    pub lighting_n_width: Box<[f32]>,
    #[mania2(0.0f32)]
    pub lighting_l_width: Box<[f32]>,

    // SPRITES

    // keycount-dependent

    // the defaults are extremely involved and i just don't want to deal with figuring this out
    // but basically it's a sequence of mania-key{1,2,S}{,D} / mania-note{1,2,S}{,H,L,T} in some particular order
    #[mania("KeyImage", "", None)]
    pub key_images: Box<[Option<Cow<'a, str>>]>,
    #[mania("KeyImage", "D", None)]
    pub key_images_d: Box<[Option<Cow<'a, str>>]>,
    #[mania("NoteImage", "", None)]
    pub note_images: Box<[Option<Cow<'a, str>>]>,
    #[mania("NoteImage", "H", None)]
    pub note_images_h: Box<[Option<Cow<'a, str>>]>,
    #[mania("NoteImage", "L", None)]
    pub note_images_l: Box<[Option<Cow<'a, str>>]>,
    #[mania("NoteImage", "T", None)]
    pub note_images_t: Box<[Option<Cow<'a, str>>]>,

    #[default(Cow::Borrowed("mania-stage-left"))]
    pub stage_left: Cow<'a, str>,
    #[default(Cow::Borrowed("mania-stage-right"))]
    pub stage_right: Cow<'a, str>,
    #[default(Cow::Borrowed("mania-stage-bottom"))]
    pub stage_bottom: Cow<'a, str>,
    #[default(Cow::Borrowed("mania-stage-hint"))]
    pub stage_hint: Cow<'a, str>,
    #[default(Cow::Borrowed("mania-stage-light"))]
    pub stage_light: Cow<'a, str>,

    #[default(Cow::Borrowed("mania-hit0"))]
    pub hit0: Cow<'a, str>,
    #[default(Cow::Borrowed("mania-hit50"))]
    pub hit50: Cow<'a, str>,
    #[default(Cow::Borrowed("mania-hit100"))]
    pub hit100: Cow<'a, str>,
    #[default(Cow::Borrowed("mania-hit200"))]
    pub hit200: Cow<'a, str>,
    #[default(Cow::Borrowed("mania-hit300"))]
    pub hit300: Cow<'a, str>,
    #[default(Cow::Borrowed("mania-hit300g"))]
    pub hit300g: Cow<'a, str>,
    #[default(Cow::Borrowed("lightingN"))]
    pub lighting_n: Cow<'a, str>,
    #[default(Cow::Borrowed("lightingL"))]
    pub lighting_l: Cow<'a, str>,
    #[default(Cow::Borrowed("mania-warningarrow"))]
    pub warning_arrow: Cow<'a, str>,

    // COLORS (i will not say colours peppy cannot force me)
    #[mania("Colour", "", (0, 0, 0, 255))]
    pub colours: Box<[ColourRgba]>,
    #[mania("ColourLight", "", (255, 255, 255, 255))]
    pub colours_light: Box<[ColourRgba]>,
    #[default((255, 255, 255, 255))]
    pub colour_column_line: ColourRgba,
    #[default((255, 255, 255, 255))]
    pub colour_judgement_line: ColourRgba,
    #[default((255, 255, 255, 255))]
    pub colour_barline: ColourRgba,
    #[default((255, 0, 0, 255))]
    pub colour_break: ColourRgba,
    #[default((255, 199, 51, 255))]
    pub colour_hold: ColourRgba,
    #[default((0, 0, 0, 255))]
    pub colour_key_warning: ColourRgba,

    // Defaults to `key_flip_when_upside_down`
    #[mania("KeyFlipWhenUpsideDown", "", None)]
    pub per_key_flip_when_upside_down: Box<[Option<bool>]>,
    // Defaults to `true`
    #[default(None)]
    pub key_flip_when_upside_down: Option<bool>,

    // !!! Values below are just not used when skin version is less than 2.5
    //     (before 2.5 most things just don't get flipped when upside down)

    // Defaults to `note_flip_when_upside_down`
    #[mania("NoteFlipWhenUpsideDown", "", None)]
    pub per_note_flip_when_upside_down: Box<[Option<bool>]>,
    // Defaults to `true`
    #[default(None)]
    pub note_flip_when_upside_down: Option<bool>,
    // Defaults to `note_flip_when_upside_down_h`
    #[mania("NoteFlipWhenUpsideDownH", "", None)]
    pub per_note_flip_when_upside_down_h: Box<[Option<bool>]>,
    // Defaults to whatever the non-H logic returned
    #[default(None)]
    pub note_flip_when_upside_down_h: Option<bool>,
    // Defaults to `note_flip_when_upside_down_l`
    #[mania("NoteFlipWhenUpsideDownL", "", None)]
    pub per_note_flip_when_upside_down_l: Box<[Option<bool>]>,
    // Defaults to `true`
    #[default(None)]
    pub note_flip_when_upside_down_l: Option<bool>,
    // Defaults to `note_flip_when_upside_down_t`
    #[mania("NoteFlipWhenUpsideDownT", "", None)]
    pub per_note_flip_when_upside_down_t: Box<[Option<bool>]>,
    // Defaults to `true`
    #[default(None)]
    pub note_flip_when_upside_down_t: Option<bool>,
    // Defaults to `note_body_style`
    #[mania("NoteBodyStyle", "", None)]
    pub per_note_body_style: Box<[Option<ManiaNoteBodyStyle>]>,
    // Defaults to `RepeatBottom`
    // (on versions < 2.5 it's always `Stretch`)
    #[default(None)]
    pub note_body_style: Option<ManiaNoteBodyStyle>,

    // !!!! End of values only used on versions >= 2.5
    #[default(0.0f32)]
    pub width_for_note_height_scale: f32,
    // min value is 5.0
    #[default(40.0f32)]
    pub stage_separation: f32,
    #[default(true)]
    pub separate_score: bool,
    #[default(None)]
    pub split_stages: Option<bool>,
    #[default(false)]
    pub keys_under_notes: bool,
    #[default(136.0f32)]
    pub column_start: f32,
    #[default(19.0f32)]
    pub column_right: f32,
    #[default(true)]
    pub judgement_line: bool,
    // Clamped between 240 and 480
    #[default(402)]
    pub hit_position: i32,
    #[default(413)]
    pub light_position: i32,
    #[default(111)]
    pub combo_position: i32,
    #[default(325)]
    pub score_position: i32,
    #[default(false)]
    pub upside_down: bool,
    // Gets set to 24 if <= 0
    #[default(60)]
    pub light_frame_per_second: i32,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct SkinIni<'a> {
    pub general: General<'a>,
    pub colours: Colours,
    pub fonts: Fonts<'a>,
    pub catch_the_beat: CatchTheBeat,
    pub mania: Vec<Mania<'a>>,
}

fn parse_line<'a, T: StaticCow<'a>>(line: T) -> Option<(T, T)> {
    if line.as_ref().starts_with("//") || line.as_ref().is_empty() {
        return None;
    }
    let line = line
        .split_once2("//")
        .map(|(a, _b)| a.trim())
        .unwrap_or(line);
    if line.as_ref().contains(':') {
        let mut line = line.split(':');
        Some((line.next().unwrap().trim(), line.next().unwrap().trim()))
    } else {
        let span = line.span();
        Some((line, T::new("", Span::new(span.end, span.end))))
    }
}

#[derive(Copy, Clone)]
enum Section {
    General,
    Colours,
    Fonts,
    CatchTheBeat,
    Mania,
}

impl SkinIni<'static> {
    pub fn parse_file(
        file: impl io::Read + io::Seek,
        ctx: &DeserializationContext,
    ) -> Result<Self, ReadError> {
        let mut file = BufReader::new(file);
        let mut section = Some(Section::General);
        let mut line0 = String::new();
        let mut line1 = String::new();
        let mut pos = 0u64;
        let mut ret = Self::default();
        loop {
            line0.clear();
            let cnt = file.read_line(&mut line0)?;
            if cnt == 0 {
                break;
            }
            let line0 = line0.trim_end_matches(['\r', '\n']);
            let line = Lended(line0, Span::new(pos, pos + line0.len() as u64));
            pos += cnt as u64;
            if line.as_ref().starts_with('[') {
                let section_name = if let Some((a, _)) = line.split_once(']') {
                    a.substr(1)
                } else {
                    line.trim_start_matches('[')
                };
                section = match section_name.as_ref() {
                    "General" => Some(Section::General),
                    "Colours" => Some(Section::Colours),
                    "Fonts" => Some(Section::Fonts),
                    "CatchTheBeat" => Some(Section::CatchTheBeat),
                    "Mania" => {
                        let mut keys = 0;
                        let mut tmp_pos = pos;
                        loop {
                            line1.clear();
                            let cnt = file.read_line(&mut line1)?;
                            if cnt == 0 || line1.starts_with("[") {
                                break;
                            }
                            let line1 = line1.trim_end_matches(['\r', '\n']);
                            let line =
                                Lended(line1, Span::new(tmp_pos, tmp_pos + line1.len() as u64));
                            tmp_pos += cnt as u64;
                            if let Some((k, v)) =
                                parse_line(line).filter(|(k, _)| k.as_ref() == "Keys")
                            {
                                match v.as_ref().parse::<u8>() {
                                    Ok(x) => {
                                        keys = x;
                                    }
                                    Err(_)
                                        if matches!(
                                            ctx.strictness,
                                            ParseStrictness::IgnoreErrors
                                        ) => {}
                                    Err(err) => {
                                        return Err(
                                            ParseError::curry(k.into_cow(), v.span())(err).into()
                                        )
                                    }
                                }
                                break;
                            }
                        }
                        if (keys == 0 || keys > 18)
                            && !matches!(ctx.strictness, ParseStrictness::IgnoreErrors)
                        {
                            return Err(ParseError::curry(line.into_cow(), line.span())(
                                MissingKeycountError,
                            )
                            .into());
                        }
                        file.seek(io::SeekFrom::Start(pos))?;
                        ret.mania.push(Mania::new(keys));
                        Some(Section::Mania)
                    }
                    _ if matches!(ctx.strictness, ParseStrictness::Strict) => {
                        return Err(ParseError::curry(line.into_cow(), line.span())(
                            UnknownSectionName,
                        )
                        .into());
                    }
                    _ => None,
                };
            } else if let Some(section) = section {
                if let Some((k, v)) = parse_line(line) {
                    match section {
                        Section::General => ret.general.consume_line(ctx, k, v)?,
                        Section::Colours => ret.colours.consume_line(ctx, k, v)?,
                        Section::Fonts => ret.fonts.consume_line(ctx, k, v)?,
                        Section::CatchTheBeat => ret.catch_the_beat.consume_line(ctx, k, v)?,
                        Section::Mania => ret.mania.last_mut().unwrap().consume_line(ctx, k, v)?,
                    }
                }
            }
        }
        Ok(ret)
    }
}
impl<'a> SkinIni<'a> {
    pub fn parse_str(data: &'a str, ctx: &DeserializationContext) -> Result<Self, ReadError> {
        let mut section = Some(Section::General);
        let mut pos = 0usize;
        let mut ret = Self::default();
        loop {
            let next_pos = memchr::memchr(b'\n', data[pos..].as_bytes())
                .map(|x| x + pos + 1)
                .unwrap_or_else(|| data.len());
            if next_pos == pos {
                break;
            }
            let line = data[pos..next_pos].trim_end_matches(['\r', '\n']);
            let line = Borrowed(line, Span::new(pos as u64, pos as u64 + line.len() as u64));
            pos = next_pos;
            if line.as_ref().starts_with('[') {
                let section_name = if let Some((a, _)) = line.split_once(']') {
                    a.substr(1)
                } else {
                    line.trim_start_matches('[')
                };
                section = match section_name.as_ref() {
                    "General" => Some(Section::General),
                    "Colours" => Some(Section::Colours),
                    "Fonts" => Some(Section::Fonts),
                    "CatchTheBeat" => Some(Section::CatchTheBeat),
                    "Mania" => {
                        let mut keys = 0;
                        let mut tmp_pos = pos;
                        loop {
                            let next_pos = memchr::memchr(b'\n', data[tmp_pos..].as_bytes())
                                .map(|x| x + tmp_pos + 1)
                                .unwrap_or_else(|| data.len());
                            if next_pos == tmp_pos {
                                break;
                            }
                            let line = data[tmp_pos..next_pos].trim_end_matches(['\r', '\n']);
                            let line = Borrowed(
                                line,
                                Span::new(tmp_pos as u64, tmp_pos as u64 + line.len() as u64),
                            );
                            tmp_pos = next_pos;
                            if let Some((k, v)) =
                                parse_line(line).filter(|(k, _)| k.as_ref() == "Keys")
                            {
                                match v.as_ref().parse::<u8>() {
                                    Ok(x) => {
                                        keys = x;
                                    }
                                    Err(_)
                                        if matches!(
                                            ctx.strictness,
                                            ParseStrictness::IgnoreErrors
                                        ) => {}
                                    Err(err) => {
                                        return Err(ParseError::curry(
                                            k.into_cow().into_owned(),
                                            v.span(),
                                        )(err)
                                        .into())
                                    }
                                }
                                break;
                            }
                        }
                        if (keys == 0 || keys > 18)
                            && !matches!(ctx.strictness, ParseStrictness::IgnoreErrors)
                        {
                            return Err(ParseError::curry(
                                line.into_cow().into_owned(),
                                line.span(),
                            )(MissingKeycountError)
                            .into());
                        }
                        ret.mania.push(Mania::new(keys));
                        Some(Section::Mania)
                    }
                    _ if matches!(ctx.strictness, ParseStrictness::Strict) => {
                        return Err(
                            ParseError::curry(line.into_cow().into_owned(), line.span())(
                                UnknownSectionName,
                            )
                            .into(),
                        );
                    }
                    _ => None,
                };
            } else if let Some(section) = section {
                if let Some((k, v)) = parse_line(line) {
                    match section {
                        Section::General => ret.general.consume_line(ctx, k, v)?,
                        Section::Colours => ret.colours.consume_line(ctx, k, v)?,
                        Section::Fonts => ret.fonts.consume_line(ctx, k, v)?,
                        Section::CatchTheBeat => ret.catch_the_beat.consume_line(ctx, k, v)?,
                        Section::Mania => ret.mania.last_mut().unwrap().consume_line(ctx, k, v)?,
                    }
                }
            }
        }
        Ok(ret)
    }
    pub fn serialize_compact(&self, out: impl io::Write) -> io::Result<()> {
        let Self {
            general,
            colours,
            fonts,
            catch_the_beat,
            mania,
        } = self;
        let mut out = io::BufWriter::new(out);
        general.serialize_compact(&mut out, general.version)?;
        if colours.should_write_compact(general.version) {
            writeln!(out, "[Colours")?;
            colours.serialize_compact(&mut out, general.version)?;
        }
        if fonts.should_write_compact(general.version) {
            writeln!(out, "[Fonts")?;
            fonts.serialize_compact(&mut out, general.version)?;
        }
        if catch_the_beat.should_write_compact(general.version) {
            writeln!(out, "[CatchTheBeat")?;
            catch_the_beat.serialize_compact(&mut out, general.version)?;
        }
        let mut encountered = [false; 19];
        for mania in mania {
            let skip = encountered[mania.keys as usize];
            encountered[mania.keys as usize] = true;
            if !skip && mania.should_write_compact(general.version) {
                writeln!(out, "[Mania")?;
                mania.serialize_compact(&mut out, general.version)?;
            }
        }
        Ok(())
    }
    pub fn serialize(&self, out: impl io::Write) -> io::Result<()> {
        let Self {
            general,
            colours,
            fonts,
            catch_the_beat,
            mania,
        } = self;
        let mut out = io::BufWriter::new(out);
        write!(out, "[General]\r\n")?;
        general.serialize(&mut out, general.version)?;
        write!(out, "\r\n[Colours]\r\n")?;
        colours.serialize(&mut out, general.version)?;
        write!(out, "\r\n[Fonts]\r\n")?;
        fonts.serialize(&mut out, general.version)?;
        write!(out, "\r\n[CatchTheBeat]\r\n")?;
        catch_the_beat.serialize(&mut out, general.version)?;
        for mania in mania {
            write!(out, "\r\n[Mania]\r\n")?;
            mania.serialize(&mut out, general.version)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const DCTX: &DeserializationContext = &DeserializationContext {
        strictness: ParseStrictness::Strict,
    };

    fn kv((k, v): (&'static str, &'static str)) -> (Borrowed<'static>, Borrowed<'static>) {
        let k = Borrowed::new(k, Span::new(0, k.len() as u64));
        let v = Borrowed::new(v, Span::new(0, v.len() as u64));
        (k, v)
    }

    fn roundtrip(skin: &SkinIni) {
        let mut a = Vec::<u8>::new();
        let mut b = Vec::<u8>::new();
        skin.serialize(&mut a).unwrap();
        skin.serialize_compact(&mut b).unwrap();
        for x in [a, b] {
            let s = String::from_utf8(x).unwrap();
            let a = SkinIni::parse_file(std::io::Cursor::new(s.as_bytes()), DCTX).unwrap();
            let b = SkinIni::parse_str(&s, DCTX).unwrap();
            eprintln!("checking file/non-file equivalence");
            assert_eq!(a, b);
            eprintln!("checking roundtrip equivalence: {s:?}");
            assert_eq!(skin.general, a.general);
            assert_eq!(skin.colours, a.colours);
            assert_eq!(skin.fonts, a.fonts);
            assert_eq!(skin.catch_the_beat, a.catch_the_beat);
            for i in 0..19 {
                assert_eq!(
                    skin.mania
                        .iter()
                        .find(|x| x.keys == i)
                        .cloned()
                        .unwrap_or_else(|| Mania::new(i)),
                    a.mania
                        .iter()
                        .find(|x| x.keys == i)
                        .cloned()
                        .unwrap_or_else(|| Mania::new(i)),
                );
            }
        }
    }

    #[test]
    fn general() {
        let mut general = General::default();
        for (k, v) in [
            ("Name", "test"),
            ("Author", "test"),
            ("SliderStyle", "1"),
            ("Version", "latest"),
            ("CustomComboBurstSounds", "1,2,3"),
            ("HitCircleOverlayAboveNumer", "0"),
            ("SpinnerFrequencyModulate", "fALSe"),
            ("SpinnerFadePlayfield", "tRUe"),
        ]
        .map(kv)
        {
            general.consume_line(DCTX, k, v).unwrap();
        }
        assert_eq!(general.name, "test");
        assert_eq!(general.author, "test");
        assert_eq!(general.slider_style, SliderStyle::PeppySliders);
        assert_eq!(general.version, None);
        assert!(!general.hit_circle_overlay_above_number);
        assert!(!general.spinner_frequency_modulate);
        assert!(general.spinner_fade_playfield);
        assert_eq!(general.custom_combo_burst_sounds, &[1, 2, 3]);
    }

    #[test]
    fn colours() {
        let mut colours = Colours::default();
        for (k, v) in [
            ("Combo1", "1,2,3,4"),
            ("Combo8", "4,5,6"),
            ("Triangle0", "1,2,3"),
            ("Triangle1", "2,3,4,5"),
        ]
        .map(kv)
        {
            colours.consume_line(DCTX, k, v).unwrap();
        }
        assert_eq!(colours.combo1, (1, 2, 3));
        assert_eq!(colours.combo2, (0, 202, 0));
        assert_eq!(colours.combo8, Some((4, 5, 6)));
        assert_eq!(colours.triangles, &[(1, 2, 3), (2, 3, 4)]);
    }

    #[test]
    fn mania() {
        let mut mania = Mania::new(4);
        assert_eq!(*mania.column_line_width, [2., 2., 2., 2.]);
        for (k, v) in [("ColumnLineWidth", "1,2,3,4"), ("KeyImage3D", "abc")].map(kv) {
            mania.consume_line(DCTX, k, v).unwrap();
        }
        assert_eq!(*mania.column_line_width, [1., 2., 3., 4.]);
        assert_eq!(
            *mania.key_images_d,
            [None, None, Some(Cow::Borrowed("abc")), None]
        );
    }

    #[test]
    fn skin() {
        let s = "Name:test  
[[[General
//test
Author:  a // test
Version: latest
SliderStyle: 4

[Colours]]abjioja]]
Combo1 : 1,2,3,4
Combo2 : 1,2,3
[Fonts]
ScoreOverlap: -3
[Mania]
Keys: 4
ColumnStart: 123
[Mania]
Keys: 5
[General
Author: the actual author
HitCircleOverlayAboveNumer:0
CustomComboBurstSounds:0
";
        eprintln!("{s:?}");
        let q = SkinIni::parse_file(std::io::Cursor::new(s.as_bytes()), DCTX).unwrap();
        let w = SkinIni::parse_str(s, DCTX).unwrap();
        assert_eq!(q, w);
        assert_eq!(q.general.name, "test");
        assert_eq!(q.general.author, "the actual author");
        assert_eq!(q.general.version, None);
        assert_eq!(q.general.slider_style, SliderStyle::OpenGlSliders);
        assert_eq!(q.colours.combo1, (1, 2, 3));
        assert_eq!(q.fonts.score_overlap, -3);
        assert_eq!(q.mania[0].keys, 4);
        assert_eq!(q.mania[0].column_start, 123.0);
        assert_eq!(q.mania[1].keys, 5);
        assert_eq!(q.mania[1].column_start, 136.0);
        roundtrip(&q);
    }
}
