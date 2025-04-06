extern crate proc_macro;
use std::collections::HashMap;

use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{Data, DeriveInput, Fields, Ident, LitStr, Meta};

fn derive_beatmap_section2(input: TokenStream) -> TokenStream {
    let input: DeriveInput = syn::parse2(input).unwrap();
    struct FieldInfo {
        kind: FieldKind,
        aliases: Vec<TokenStream>,
    }
    enum FieldKind {
        None,
        FromI32,
    }
    let mut fields = HashMap::<Ident, FieldInfo>::new();
    let name = input.ident;
    let generics = input.generics;
    let Data::Struct(data) = input.data else {
        panic!("#[derive(BeatmapSection)] is only allowed on structs")
    };
    let Fields::Named(named) = data.fields else {
        panic!("#[derive(BeatmapSection)] is only allowed on named fields")
    };
    let mut extra_handler = None;
    for attr in input.attrs {
        if attr.meta.path().to_token_stream().to_string() != "beatmap_section" {
            continue;
        }
        match attr.meta {
            Meta::List(x) => {
                extra_handler = Some(x.tokens);
            }
            _ => panic!("#[derive(BeatmapSection)]: non-Meta::List"),
        }
    }
    for field in named.named {
        let mut kind = FieldKind::None;
        let mut aliases = vec![];
        for attr in field.attrs {
            match attr.meta.path().to_token_stream().to_string().as_str() {
                "from" => match attr.meta {
                    Meta::List(x) => {
                        let mut tokens = x.tokens.into_iter();
                        let ty = tokens.next();
                        if let Some(ty) = ty {
                            let ty = ty.to_string();
                            assert_eq!(ty, "i32");
                            kind = FieldKind::FromI32;
                            assert!(tokens.next().is_none());
                        }
                    }
                    _ => panic!("#[derive(BeatmapSection)]: non-Meta::List"),
                },
                "alias" => match attr.meta {
                    Meta::List(x) => {
                        aliases.push(x.tokens);
                    }
                    _ => panic!("#[derive(BeatmapSection)]: non-Meta::List"),
                },
                "doc" => {}
                x => panic!("{x}"),
            }
        }
        assert!(fields
            .insert(
                field.ident.expect("field ident"),
                FieldInfo { kind, aliases }
            )
            .is_none());
    }
    let mut match_fields = TokenStream::new();
    let mut valid_fields = TokenStream::new();
    for (name, info) in fields {
        let name_camel = name
            .to_string()
            .split('_')
            .map(|x| {
                let mut first = true;
                if matches!(x, "id" | "hp") {
                    x.to_uppercase()
                } else {
                    x.chars()
                        .map(|x| {
                            if first {
                                first = false;
                                x.to_ascii_uppercase()
                            } else {
                                x
                            }
                        })
                        .collect::<String>()
                }
            })
            .collect::<Vec<_>>()
            .join("");
        let lit = syn::LitStr::new(&name_camel, proc_macro2::Span::call_site()).into_token_stream();
        for lit in [lit].into_iter().chain(info.aliases) {
            match info.kind {
                FieldKind::None => {
                    match_fields.extend(quote! {
                        #lit => {
                            self.#name = ParseField::parse_field(#lit, ctx, value)?;
                            return Ok(None);
                        }
                    });
                }
                FieldKind::FromI32 => {
                    match_fields.extend(quote! {
                        #lit => {
                            self.#name = {
                                TryFrom::try_from(i32::parse_field(#lit, ctx, value)?)
                                    .map_err(ParseError::curry(#lit, value.span()))?
                            };
                            return Ok(None);
                        }
                    });
                }
            }
            valid_fields.extend(quote! { #lit, });
        }
    }
    let name_str = LitStr::new(&(name.to_string() + " section"), Span::call_site());
    let default_handler = quote! {
        {
            return Err(ParseError::curry(#name_str, line.span())(RecordParseError {
                valid_fields: &[#valid_fields],
            }));
        }
    };
    let extra_handler = if let Some(handler) = extra_handler {
        quote! {
            {
                if let Some(ret) = #handler(key) {
                    return Ok(Some(ret));
                } else #default_handler
            }
        }
    } else {
        default_handler
    };
    quote! {
        impl<'a> BeatmapSection<'a> for #name #generics {
            fn consume_line(
                &mut self,
                ctx: &Context,
                line: impl StaticCow<'a>,
            ) -> Result<Option<Section>, ParseError<'static>> {
                if let Some((key, value)) = line.split_once(':') {
                    let key = key.trim();
                    let value = value.trim();
                    match key.as_ref() {
                        #match_fields
                        _ => #extra_handler
                    }
                } else {
                    Err(ParseError::curry(#name_str, line.span())(InvalidRecordField))
                }
            }
        }
    }
}

// its similar to derive_beatmap_section but different enough for me to just completely split it
fn derive_skin_section2(input: TokenStream) -> TokenStream {
    let input: DeriveInput = syn::parse2(input).unwrap();
    struct FieldInfo {
        kind: FieldKind,
        to_i32: bool,
        aliases: Vec<TokenStream>,
    }
    enum FieldKind {
        Skip,
        Default(TokenStream),
        Version,
        Mania(TokenStream, Option<(String, String)>),
        NumPrefix(String),
    }
    let mut mania = false;
    let mut fields = HashMap::<Ident, FieldInfo>::new();
    let name = input.ident;
    let generics = input.generics;
    let Data::Struct(data) = input.data else {
        panic!("#[derive(SkinSection)] is only allowed on structs")
    };
    let Fields::Named(named) = data.fields else {
        panic!("#[derive(SkinSection)] is only allowed on named fields")
    };
    for field in named.named {
        let mut aliases = vec![];
        let mut kind = None;
        let mut to_i32 = false;
        for attr in field.attrs {
            match attr.meta.path().to_token_stream().to_string().as_str() {
                "skip" => {
                    assert!(kind.is_none());
                    kind = Some(FieldKind::Skip);
                }
                "alias" => match attr.meta {
                    Meta::List(x) => {
                        aliases.push(x.tokens);
                    }
                    _ => panic!("#[derive(SkinSection)]: non-Meta::List"),
                },
                "default" => match attr.meta {
                    Meta::List(x) => {
                        assert!(kind.is_none());
                        kind = Some(FieldKind::Default(x.tokens));
                    }
                    _ => panic!("#[derive(SkinSection)]: non-Meta::List"),
                },
                "version" => {
                    assert!(kind.is_none());
                    kind = Some(FieldKind::Version);
                }
                "mania" => match attr.meta {
                    Meta::List(x) => {
                        mania = true;
                        assert!(kind.is_none());
                        let mut t = x.tokens.into_iter();
                        let t0 = t.next().unwrap();
                        let t1 = t.next().unwrap();
                        let t2 = t.next().unwrap();
                        let t3 = t.next().unwrap();
                        let t4 = t.next().unwrap();
                        assert_eq!(t1.to_string().as_str(), ",");
                        assert_eq!(t3.to_string().as_str(), ",");
                        let t0: LitStr = syn::parse2(t0.into_token_stream()).unwrap();
                        let t2: LitStr = syn::parse2(t2.into_token_stream()).unwrap();
                        kind = Some(FieldKind::Mania(
                            t4.into_token_stream(),
                            Some((t0.value(), t2.value())),
                        ));
                    }
                    _ => panic!("#[derive(SkinSection)]: non-Meta::List"),
                },
                "mania2" => match attr.meta {
                    Meta::List(x) => {
                        mania = true;
                        assert!(kind.is_none());
                        kind = Some(FieldKind::Mania(x.tokens, None));
                    }
                    _ => panic!("#[derive(SkinSection)]: non-Meta::List"),
                },
                "to" => match attr.meta {
                    Meta::List(x) => {
                        let mut tokens = x.tokens.into_iter();
                        let ty = tokens.next();
                        if let Some(ty) = ty {
                            let ty = ty.to_string();
                            assert_eq!(ty, "i32");
                            to_i32 = true;
                            assert!(tokens.next().is_none());
                        }
                    }
                    _ => panic!("#[derive(SkinSection)]: non-Meta::List"),
                },
                "number_with_prefix" => match attr.meta {
                    Meta::List(x) => {
                        assert!(kind.is_none());
                        let mut tokens = x.tokens.into_iter();
                        let s = tokens.next();
                        let s: LitStr = syn::parse2(s.into_token_stream()).unwrap();
                        assert!(tokens.next().is_none());
                        kind = Some(FieldKind::NumPrefix(s.value()));
                    }
                    _ => panic!("#[derive(SkinSection)]: non-Meta::List"),
                },
                "doc" => {}
                x => panic!("{x}"),
            }
        }
        let kind = kind.unwrap();
        assert!(fields
            .insert(
                field.ident.expect("field ident"),
                FieldInfo {
                    kind,
                    aliases,
                    to_i32
                }
            )
            .is_none());
    }
    let mut match_fields = TokenStream::new();
    let mut valid_fields = TokenStream::new();
    let mut default_fields = TokenStream::new();
    let mut ser = TokenStream::new();
    let mut ser_compact = TokenStream::new();
    for (name, info) in fields {
        let name_camel = name
            .to_string()
            .split('_')
            .map(|x| {
                let mut first = true;
                x.chars()
                    .map(|x| {
                        if first {
                            first = false;
                            x.to_ascii_uppercase()
                        } else {
                            x
                        }
                    })
                    .collect::<String>()
            })
            .collect::<Vec<_>>()
            .join("");
        let lit = syn::LitStr::new(&name_camel, proc_macro2::Span::call_site()).into_token_stream();
        for lit in [lit].into_iter().chain(info.aliases) {
            match info.kind {
                FieldKind::Skip => continue,
                FieldKind::Default(_) | FieldKind::Mania(_, None) => {
                    match_fields.extend(quote! {
                        #lit => {
                            self.#name = ParseField::parse_field(#lit, value)?;
                            Ok(())
                        }
                    });
                    valid_fields.extend(quote! { #lit, });
                }
                FieldKind::Version => {
                    match_fields.extend(quote! {
                        #lit => {
                            if value.as_ref() == "latest" {
                                self.#name = None;
                            } else {
                                self.#name = Some(ParseField::parse_field(#lit, value)?);
                            }
                            Ok(())
                        }
                    });
                    valid_fields.extend(quote! { #lit, });
                }
                FieldKind::Mania(_, Some((ref pre, ref post))) => {
                    for i in 0usize..18 {
                        let mut lit = pre.clone();
                        lit.push_str(&(i + 1).to_string());
                        lit.push_str(post);
                        valid_fields.extend(quote! { #lit, });
                        match_fields.extend(quote! {
                            #lit if #i < self.#name.len() => {
                                self.#name[#i] = ParseField::parse_field(#lit, value)?;
                                Ok(())
                            }
                        });
                    }
                }
                FieldKind::NumPrefix(ref s) => {
                    match_fields.extend(quote! {
                        s if matches!(s.strip_prefix(#s), Some(x) if x.bytes().all(|x| x.is_ascii_digit())) => {
                            let s = s.strip_prefix(#s).unwrap();
                            self.#name.push(ParseField::parse_field(s, value)?);
                            Ok(())
                        }
                    })
                }
            }
        }
        match info.kind {
            FieldKind::Skip => {}
            FieldKind::Version => {
                default_fields.extend(quote! {
                    #name: Some(1.0),
                });
            }
            FieldKind::Default(def) => {
                default_fields.extend(quote! {
                    #name: #def,
                });
            }
            FieldKind::NumPrefix(_) => {
                default_fields.extend(quote! {
                    #name: vec![],
                });
            }
            FieldKind::Mania(def, _) => {
                default_fields.extend(quote! {
                    #name: vec![#def; keys as usize],
                });
            }
        }
    }
    let name_str = LitStr::new(&(name.to_string() + " section"), Span::call_site());
    let extra_handler = quote! {
        {
            if ctx.strict {
                Err(ParseError::curry(#name_str, value.span())(RecordParseError {
                    valid_fields: &[#valid_fields],
                }))
            } else {
                Ok(())
            }
        }
    };
    let mut ret = quote! {
        impl<'a> SkinSection<'a> for #name #generics {
            fn consume_line(
                &mut self,
                ctx: &DeserializationContext,
                key: &'a str,
                value: impl StaticCow<'a>
            ) -> Result<(), ParseError<'a>> {
                match key.as_ref() {
                    #match_fields
                    _ => #extra_handler
                }
            }
        }
    };
    if mania {
        ret.extend(quote! {
            impl #generics #name #generics {
                fn serialize(&self, out: impl io::Write) -> io::Result<()> {
                    #ser
                    Ok(())
                }
                fn serialize_compact(&self, out: impl io::Write) -> io::Result<()> {
                    #ser_compact
                    Ok(())
                }
                fn default_for(keys: u8) -> Self {
                    Self {
                        keys,
                        #default_fields
                    }
                }
            }
        });
    } else {
        ret.extend(quote! {
            impl #generics Default for #name #generics {
                fn default() -> Self {
                    Self {
                        #default_fields
                    }
                }
            }
            impl #generics #name #generics {
                fn serialize(&self, out: impl io::Write) -> io::Result<()> {
                    #ser
                    Ok(())
                }
                fn serialize_compact(&self, out: impl io::Write) -> io::Result<()> {
                    #ser_compact
                    Ok(())
                }
            }
        });
    }
    ret
}

fn derive_enum2(input: TokenStream, beatmap: bool) -> TokenStream {
    let input: DeriveInput = syn::parse2(input).unwrap();
    let name = input.ident;
    let mut ignore_case = false;
    let mut from_char = false;
    for attr in input.attrs {
        if !matches!(
            attr.meta.path().to_token_stream().to_string().as_str(),
            "beatmap_enum"
        ) {
            continue;
        }
        match attr.meta {
            Meta::List(x) => {
                let mut tokens = x.tokens.into_iter();
                let attr = tokens.next().unwrap().to_string();
                assert!(tokens.next().is_none());
                match attr.as_str() {
                    "ignore_case" => {
                        ignore_case = true;
                    }
                    "from_char" => {
                        from_char = true;
                    }
                    x => panic!("#[derive(BeatmapEnum)]: unexpected attr contents {x}"),
                }
            }
            _ => panic!("#[derive(BeatmapEnum)]: non-Meta::List"),
        }
    }
    let Data::Enum(data) = input.data else {
        panic!("#[derive(BeatmapEnum)] is only allowed on enums")
    };
    let mut match_fields = TokenStream::new();
    let mut int_match_fields = TokenStream::new();
    let mut char_match_fields = TokenStream::new();
    let mut reverse_char_match = TokenStream::new();
    let mut reverse_match = TokenStream::new();
    let mut valid_variants = TokenStream::new();
    let mut valid_int_variants = TokenStream::new();
    let mut valid_char_variants = TokenStream::new();
    let name0 = &name;
    for field in data.variants {
        assert!(field.fields.is_empty());
        let name = field.ident;
        let mut name1 = name.to_string();
        if ignore_case {
            name1.make_ascii_lowercase();
        }
        let lit = syn::LitStr::new(&name1, proc_macro2::Span::call_site());
        match_fields.extend(quote! {
            #lit => Ok(Self::#name),
        });
        reverse_match.extend(quote! {
            #name0::#name => #lit,
        });
        valid_variants.extend(quote! {
            #lit,
        });
        let char_lit = syn::LitChar::new(
            name1.chars().next().unwrap(),
            proc_macro2::Span::call_site(),
        );
        char_match_fields.extend(quote! {
            #char_lit => Ok(Self::#name),
        });
        reverse_char_match.extend(quote! {
            #name0::#name => #char_lit,
        });
        valid_char_variants.extend(quote! {
            #char_lit,
        });
        if let Some((_, discrim)) = field.discriminant {
            int_match_fields.extend(quote! {
                #discrim => Ok(Self::#name),
            });
            valid_int_variants.extend(quote! {
                #discrim,
            });
            let str_discrim = discrim.into_token_stream().to_string();
            let lit = syn::LitStr::new(&str_discrim, proc_macro2::Span::call_site());
            match_fields.extend(quote! {
                #lit => Ok(Self::#name),
            });
        }
    }
    let scrutinee = if ignore_case {
        quote! {
            s.to_lowercase().as_str()
        }
    } else {
        quote! {
            s
        }
    };
    let mut extra = quote! {};
    if from_char {
        extra.extend(quote! {
            impl std::convert::TryFrom<char> for #name {
                type Error = CharEnumParseError;
                fn try_from(x: char) -> Result<Self, Self::Error> {
                    match x {
                        #char_match_fields
                        _ => Err(CharEnumParseError {
                            variant: x,
                            valid_variants: &[#valid_char_variants],
                        }),
                    }
                }
            }
            impl From<#name> for char {
                fn from(x: #name) -> Self {
                    match x {
                        #reverse_char_match
                    }
                }
            }
        });
    }
    extra.extend(if beatmap {
        quote! {
            impl<'a> ParseField<'a> for #name {
                fn parse_field(
                    name: impl Into<Cow<'static, str>>,
                    _ctx: &Context,
                    line: impl StaticCow<'a>,
                ) -> Result<Self, ParseError<'static>> {
                    line.as_ref().parse().map_err(ParseError::curry(name, line.span()))
                }
            }
        }
    } else {
        quote! {
            impl<'a> ParseField<'a> for #name {
                fn parse_field(
                    name: &'a str,
                    line: impl StaticCow<'a>,
                ) -> Result<Self, ParseError<'a>> {
                    line.as_ref().parse().map_err(ParseError::curry(name, line.span()))
                }
            }
        }
    });
    quote! {
        #extra
        impl std::fmt::Display for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                f.write_str(match self {
                    #reverse_match
                })
            }
        }
        impl std::convert::TryFrom<i32> for #name {
            type Error = IntEnumParseError;
            fn try_from(x: i32) -> Result<Self, Self::Error> {
                match x {
                    #int_match_fields
                    _ => Err(IntEnumParseError {
                        variant: x,
                        valid_variants: &[#valid_int_variants],
                    }),
                }
            }
        }
        impl std::str::FromStr for #name {
            type Err = EnumParseError;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match #scrutinee {
                    #match_fields
                    _ => Err(EnumParseError {
                        valid_variants: &[#valid_variants],
                    }),
                }
            }
        }
    }
}

#[proc_macro_derive(BeatmapSection, attributes(from, alias, beatmap_section))]
pub fn derive_beatmap_section(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_beatmap_section2(input.into()).into()
}

#[proc_macro_derive(
    SkinSection,
    attributes(default, version, alias, mania, mania2, skip, to, number_with_prefix)
)]
pub fn derive_skin_section(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_skin_section2(input.into()).into()
}

#[proc_macro_derive(BeatmapEnum, attributes(beatmap_enum))]
pub fn derive_beatmap_enum(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_enum2(input.into(), true).into()
}

#[proc_macro_derive(SkinEnum, attributes(beatmap_enum))]
pub fn derive_skin_enum(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_enum2(input.into(), false).into()
}
