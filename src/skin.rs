//! skin.ini parsing and serialization.

use std::{borrow::Cow, io};

use osu_rs_derive::{SkinEnum, SkinSection};

use crate::{
    parsers::skin::ParseField, util::StaticCow, EnumParseError, IntEnumParseError, ParseError,
    RecordParseError,
};

pub type ColourRgb = (u8, u8, u8);
pub type ColourRgba = (u8, u8, u8, u8);

struct SerializationContext {
    pub compact: bool,
}

struct DeserializationContext {
    pub strict: bool,
}

trait SkinSection<'a> {
    fn consume_line(
        &mut self,
        ctx: &DeserializationContext,
        key: &'a str,
        value: impl StaticCow<'a>,
    ) -> Result<(), ParseError<'a>>;
}

#[derive(SkinEnum, Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SliderStyle {
    PeppySliders = 1,
    MmSliders = 2,
    ToonSliders = 3,
    OpenGlSliders = 4,
}

#[derive(SkinSection)]
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
    #[to(i32)]
    pub slider_style: SliderStyle,
    /// Latest version if None
    #[version]
    pub version: Option<f64>,
}

#[derive(SkinSection)]
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

#[derive(SkinSection)]
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

#[derive(SkinSection)]
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

#[derive(SkinSection)]
pub struct Mania<'a> {
    /// From 0 to 18 (inclusive).
    #[skip]
    pub keys: u8,
    /// Note that everything must be <= 0 or >= 2 (otherwise it gets replaced with 2)
    #[mania2(2.0f32)]
    pub column_line_width: Vec<f32>,
    #[default(1.2f32)]
    pub barline_height: f32,
    #[default(ManiaSpecialStyle::None)]
    pub special_style: ManiaSpecialStyle,
    /// Clamped between 5 and 100
    #[mania2(30.0f32)]
    pub column_width: Vec<f32>,
    // `column_spacing[i] >= -column_width[i + 1]` must be upheld for all `i` under `keys - 1`
    #[mania2(0.0f32)]
    pub column_spacing: Vec<f32>,
    #[mania2(0.0f32)]
    pub lighting_n_width: Vec<f32>,
    #[mania2(0.0f32)]
    pub lighting_l_width: Vec<f32>,

    // SPRITES

    // keycount-dependent

    // the defaults are extremely involved and i just don't want to deal with figuring this out
    // but basically it's a sequence of mania-key{1,2,S}{,D} / mania-note{1,2,S}{,H,L,T} in some particular order
    #[mania("KeyImage", "", None)]
    pub key_images: Vec<Option<Cow<'a, str>>>,
    #[mania("KeyImage", "D", None)]
    pub key_images_d: Vec<Option<Cow<'a, str>>>,
    #[mania("NoteImage", "", None)]
    pub note_images: Vec<Option<Cow<'a, str>>>,
    #[mania("NoteImage", "H", None)]
    pub note_images_h: Vec<Option<Cow<'a, str>>>,
    #[mania("NoteImage", "L", None)]
    pub note_images_l: Vec<Option<Cow<'a, str>>>,
    #[mania("NoteImage", "T", None)]
    pub note_images_t: Vec<Option<Cow<'a, str>>>,

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
    pub colours: Vec<ColourRgba>,
    #[mania("ColourLight", "", (255, 255, 255, 255))]
    pub colours_light: Vec<ColourRgba>,
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
    pub per_key_flip_when_upside_down: Vec<Option<bool>>,
    // Actual default value depends on skin version
    #[default(None)]
    pub key_flip_when_upside_down: Option<bool>,

    // !!! Values below are just not used when skin version is less than 2.5
    //     (before 2.5 most things just don't get flipped when upside down)

    // Defaults to `note_flip_when_upside_down`
    #[mania("NoteFlipWhenUpsideDown", "", None)]
    pub per_note_flip_when_upside_down: Vec<Option<bool>>,
    // Defaults to `true`
    #[default(None)]
    pub note_flip_when_upside_down: Option<bool>,
    // Defaults to `note_flip_when_upside_down_h`
    #[mania("NoteFlipWhenUpsideDownH", "", None)]
    pub per_note_flip_when_upside_down_h: Vec<Option<bool>>,
    // Defaults to whatever the non-H logic returned
    #[default(None)]
    pub note_flip_when_upside_down_h: Option<bool>,
    // Defaults to `note_flip_when_upside_down_l`
    #[mania("NoteFlipWhenUpsideDownL", "", None)]
    pub per_note_flip_when_upside_down_l: Vec<Option<bool>>,
    // Defaults to `true`
    #[default(None)]
    pub note_flip_when_upside_down_l: Option<bool>,
    // Defaults to `note_flip_when_upside_down_t`
    #[mania("NoteFlipWhenUpsideDownT", "", None)]
    pub per_note_flip_when_upside_down_t: Vec<Option<bool>>,
    // Defaults to `true`
    #[default(None)]
    pub note_flip_when_upside_down_t: Option<bool>,
    // Defaults to `note_body_style`
    #[mania("NoteBodyStyle", "", None)]
    pub per_note_body_style: Vec<Option<ManiaNoteBodyStyle>>,
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

#[cfg(test)]
mod test {
    use crate::{util::Borrowed, Span};

    use super::*;

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
        ] {
            general
                .consume_line(
                    &DeserializationContext { strict: true },
                    k,
                    Borrowed(v, Span::new(0, v.len())),
                )
                .unwrap();
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
        ] {
            colours
                .consume_line(
                    &DeserializationContext { strict: true },
                    k,
                    Borrowed(v, Span::new(0, v.len())),
                )
                .unwrap();
        }
        assert_eq!(colours.combo1, (1, 2, 3));
        assert_eq!(colours.combo2, (0, 202, 0));
        assert_eq!(colours.combo8, Some((4, 5, 6)));
        assert_eq!(colours.triangles, &[(1, 2, 3), (2, 3, 4)]);
    }

    #[test]
    fn mania() {
        let mut mania = Mania::default_for(4);
        assert_eq!(mania.column_line_width, &[2., 2., 2., 2.]);
        for (k, v) in [("ColumnLineWidth", "1,2,3,4"), ("KeyImage3D", "abc")] {
            mania
                .consume_line(
                    &DeserializationContext { strict: true },
                    k,
                    Borrowed(v, Span::new(0, v.len())),
                )
                .unwrap();
        }
        assert_eq!(mania.column_line_width, &[1., 2., 3., 4.]);
        assert_eq!(
            mania.key_images_d,
            &[None, None, Some(Cow::Borrowed("abc")), None]
        );
    }
}
