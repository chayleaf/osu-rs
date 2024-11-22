[![crates.io](https://img.shields.io/crates/v/osu-rs.svg)](https://crates.io/crates/osu-rs)
[![docs.rs](https://docs.rs/osu-rs/badge.svg)](https://docs.rs/osu-rs)

# osu-rs

I have no idea what other osu! file format parsers there are, but this
one is mine!

The focus here was on performance so I tried to do 0-alloc
deserialization where possible. I've tested roundtrips on a lot of
beatmaps with different format versions so I'm reasonably sure this
should cover most of them (roundtrips aren't byte-by-byte perfect all of
the time because there's lots of leeway, but it's reasonably close at
least).

Sorry for having very sparse documentation, I'll improve it later...
hopefully
