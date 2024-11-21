[![crates.io](https://img.shields.io/crates/v/osu-rs.svg)](https://crates.io/crates/osu-rs)
[![docs.rs](https://docs.rs/osu-rs/badge.svg)](https://docs.rs/osu-rs)

# osu-rs

I have no idea what other osu! file format parsers there are, but this
one is mine!

The focus here was on performance so I tried to do 0-alloc
deserialization where possible. Only rudimentary .osu file reading is
supported for now (this means you can't read storyboards, but can
hopefully read literally anything else, even though I haven't tested
much). Additionally, I use spanned strings instead of normal strings
everywhere, so you get free error reporting as a bonus.

Serialization also works, but only for the current format version.

Sorry for having very sparse documentation, I'll improve it later...
hopefully
