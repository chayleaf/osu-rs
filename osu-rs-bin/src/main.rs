use std::{os::unix::ffi::OsStrExt, path::Path};

use ariadne::{Label, Report, ReportKind, Source};

fn read_beatmap(filename: impl AsRef<Path>) -> osu_rs::Beatmap<'static> {
    let file = std::fs::File::open(&filename).expect("file open error");
    let reader = std::io::BufReader::new(file);
    match osu_rs::Beatmap::parse_file(reader) {
        Ok(beatmap) => beatmap,
        Err(err) => {
            let err = match err {
                osu_rs::ReadError::Io(err) => panic!("io error: {err}"),
                osu_rs::ReadError::Parse(x) => x,
            };
            let text = std::fs::read_to_string(&filename).unwrap();

            let filename = "";
            Report::build(ReportKind::Error, (filename, err.span.into_range()))
                .with_message(format!("failed to parse {}", err.field))
                .with_label(
                    Label::new((filename, err.span.into_range()))
                        .with_message(format!("{}", err.reason)),
                )
                .finish()
                .print((filename, Source::from(text.as_str())))
                .unwrap();
            std::process::exit(1);
        }
    }
}

fn parse_beatmap(text: &str) -> osu_rs::Beatmap {
    match osu_rs::Beatmap::parse_str(text) {
        Ok(beatmap) => beatmap,
        Err(err) => {
            let filename = "";
            Report::build(ReportKind::Error, (filename, err.span.into_range()))
                .with_message(format!("failed to parse {}", err.field))
                .with_label(
                    Label::new((filename, err.span.into_range()))
                        .with_message(format!("{}", err.reason)),
                )
                .finish()
                .print((filename, Source::from(text)))
                .unwrap();
            std::process::exit(1);
        }
    }
}

fn main() {
    let cmd = std::env::args().nth(1).expect("expected cmd");
    match cmd.as_str() {
        "test" => {
            let filename = std::env::args_os().nth(2).expect("expected filename");
            let bm = read_beatmap(filename);
            println!("{bm:#?}");
        }
        "write" => {
            let filename = std::env::args_os().nth(2).expect("expected filename");
            let out = std::env::args_os().nth(3).expect("expected out filename");
            let bm = read_beatmap(filename);
            bm.serialize(std::fs::File::create(out).unwrap()).unwrap();
        }
        "verify" => {
            let dir = std::env::args_os().nth(2).expect("expected dir");
            for entry in walkdir::WalkDir::new(dir) {
                let entry = entry.unwrap();
                if entry
                    .path()
                    .extension()
                    .is_some_and(|x| x.as_bytes() == b"osu")
                {
                    println!("verifying {:?}", entry.file_name());
                    let text = std::fs::read_to_string(entry.path()).unwrap();
                    let bm = parse_beatmap(&text);
                    let mut out = Vec::new();
                    bm.serialize(&mut out).unwrap();
                    let mut pos = out.len().min(text.len());
                    for (i, (a, b)) in text.bytes().zip(&out).enumerate() {
                        if a != *b {
                            pos = i;
                            break;
                        }
                    }
                    while !text.is_char_boundary(pos) {
                        pos -= 1;
                    }
                    let span = pos..pos;
                    if out != text.as_bytes() {
                        Report::build(
                            ReportKind::Error,
                            (entry.file_name().to_string_lossy(), span.clone()),
                        )
                        .with_message("mismatch")
                        .with_label(
                            Label::new((entry.file_name().to_string_lossy(), span.clone()))
                                .with_message("mismatch"),
                        )
                        .finish()
                        .print((entry.file_name().to_string_lossy(), Source::from(text)))
                        .unwrap();
                        std::process::exit(1);
                    }
                }
            }
        }
        "merge" => {
            let dir = std::env::args_os().nth(2).expect("expected dir");
            let out = std::env::args_os().nth(3).expect("expected out filename");
            let mut names = vec![];
            for entry in std::fs::read_dir(dir).unwrap() {
                let entry = entry.unwrap();
                // hack
                if entry.file_name().to_string_lossy().contains("[3]") {
                    continue;
                }
                if entry
                    .path()
                    .extension()
                    .is_some_and(|x| x.as_bytes() == b"osu")
                {
                    names.push(entry.path());
                }
            }
            names.sort();
            let merged = names
                .into_iter()
                .fold(None::<osu_rs::Beatmap<'static>>, |old, new| {
                    let mut new = read_beatmap(new);
                    if let Some(mut old) = old {
                        assert!(
                            old.timing_points.last().unwrap().offset
                                < new.timing_points.first().unwrap().offset
                        );
                        assert!(
                            old.hit_objects.last().unwrap().time
                                < new.hit_objects.first().unwrap().time
                        );
                        old.timing_points.extend(new.timing_points);
                        old.hit_objects.extend(new.hit_objects);
                        old.editor.bookmarks.extend(new.editor.bookmarks);
                        Some(old)
                    } else {
                        new.metadata.version = ":3".into();
                        Some(new)
                    }
                })
                .unwrap();
            merged
                .serialize(std::fs::File::create(out).unwrap())
                .unwrap();
        }
        _ => panic!("unknown cmd"),
    }
}
