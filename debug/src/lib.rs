#![allow(dead_code, unused_imports)]
use std::{fmt::{Debug, self}, marker::PhantomData, collections::HashMap};

use proc_macro::TokenStream;
use syn::{DeriveInput, parse_macro_input, parse_quote, visit::{Visit, self}, TypePath};
use quote::quote;

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = parse_macro_input!(input as DeriveInput);

    match do_expand(&st) {
        Ok(rst) => rst.into(),
        Err(e) => e.into_compile_error().into(),
    }
    
}

fn do_expand(st: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let ret = generate_debug_trait(st)?;

    Ok(ret)
}

type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token![,]>;
fn get_fields_from_derive_input(st: &DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(
        syn::DataStruct {
            fields: syn::Fields::Named(
                syn::FieldsNamed {
                    ref named,
                    ..
                }
            ),
            ..
        }
    ) = st.data {
        return Ok(named)
    }

    Err(syn::Error::new_spanned(st, "Must define on a Struct, not Enum".to_string()))
}

fn generate_debug_trait_core(st :&syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name_ident = &st.ident;
    let struct_name_literal = struct_name_ident.to_string();
    
    let mut fmt_body_stream = proc_macro2::TokenStream::new();
    fmt_body_stream.extend(quote!(
        fmt.debug_struct(#struct_name_literal)
    ));

    let fields = get_fields_from_derive_input(st)?;
    for field in fields {
        let field_name_ident = field.ident.as_ref().unwrap();
        let field_name_literal = field_name_ident.to_string();
        let mut format_str = "{:?}".to_owned();
        
        if let Some(format) = get_custom_format_of_field(field)? {
            format_str = format;
        }

        fmt_body_stream.extend(quote!(
            .field(#field_name_literal, &format_args!(#format_str, self.#field_name_ident))
        ));
    }

    fmt_body_stream.extend(quote!(
        .finish()
    ));

    Ok(fmt_body_stream)
}

fn generate_debug_trait(st: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name_ident = &st.ident;
    let fmt_body_stream = generate_debug_trait_core(st)?;

    let fields = get_fields_from_derive_input(st)?;
    let mut field_type_names = Vec::new();
    let mut phantomdata_type_param_names = Vec::new();
    for field in fields{
        if let Some(s) = get_field_type_name(field)? {
            field_type_names.push(s);
        }
        if let Some(s) = get_phantomdata_generic_type_name(field)? {
            phantomdata_type_param_names.push(s);
        }
    }

    let mut generics_param_to_modify = st.generics.clone();
    if let Some(hatch) = get_struct_escape_hatch(st) {
        generics_param_to_modify.make_where_clause().predicates.push(syn::parse_str(&hatch).unwrap());
    } else {
        let associated_types_map = get_generic_associated_types(st);
        for g in generics_param_to_modify.params.iter_mut() {
            if let syn::GenericParam::Type(t) = g {
                let type_param_name = t.ident.to_string();
                if phantomdata_type_param_names.contains(&type_param_name) && !field_type_names.contains(&type_param_name) {
                    continue; 
                }

                // 对有关联类型的泛型参数不添加 Debug trait bounds
                if associated_types_map.contains_key(&type_param_name) && !field_type_names.contains(&type_param_name) {
                    continue;
                }

                t.bounds.push(parse_quote!(std::fmt::Debug));
            }
        }

        // 对所有的泛型参数的关联类型添加 Debug trait bounds
        let where_clause = generics_param_to_modify.make_where_clause();
        associated_types_map.iter().for_each(|(_, associated_types)| {
            associated_types.iter().for_each(|associated_type| {
                where_clause.predicates.push(parse_quote!(#associated_type: std::fmt::Debug))
            });
        });
    }

    let (impl_generics, ty_generics, where_clause) = generics_param_to_modify.split_for_impl();
    // eprintln!("{:#?}, {:#?}, {:#?}", impl_generics, ty_generics, where_clause);
    let ret = quote!(
        impl #impl_generics core::fmt::Debug for #struct_name_ident #ty_generics #where_clause {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                #fmt_body_stream
            }
        }
    );

    Ok(ret)
}

fn get_custom_format_of_field(field: &syn::Field) -> syn::Result<Option<String>> {
    if let Some(attr) = field.attrs.first() {
        if let Ok(syn::Meta::NameValue(syn::MetaNameValue { ref path, ref lit, .. })) = attr.parse_meta() {
            if path.is_ident("debug") {
                if let syn::Lit::Str(ref token) = lit {
                    return Ok(Some(token.value()))
                }
            }
        }
    }

    Ok(None)
}

fn get_phantomdata_generic_type_name(field: &syn::Field) -> syn::Result<Option<String>> {
    // eprintln!("{:#?}", field);
    if let syn::Type::Path(syn::TypePath { ref path, .. }) = &field.ty {
        if let Some(syn::PathSegment{ ref ident, ref arguments }) = path.segments.last() {
            if ident == "PhantomData" {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments{ ref args, .. }) = arguments { 
                    if let Some(syn::GenericArgument::Type(syn::Type::Path(syn::TypePath { ref path, .. }))) = args.first() {
                        if let Some(ident) = path.get_ident() {
                            return Ok(Some(ident.to_string()))
                        }
                    }
                }
            }
        }
    }

    Ok(None)
}

fn get_field_type_name(field: &syn::Field) -> syn::Result<Option<String>> {
    if let syn::Type::Path(syn::TypePath { ref path, .. }) = &field.ty {
        if let Some(ident) = path.get_ident() {
            return Ok(Some(ident.to_string()))
        }
    }

    return Ok(None)
}

struct TypePathVisitor {
    generic_type_names: Vec<String>,
    associated_types: HashMap<String, Vec<syn::TypePath>>
}

impl<'ast> Visit<'ast> for TypePathVisitor {
    fn visit_type_path(&mut self, node: &'ast TypePath) {
        if node.path.segments.len() >= 2 {
            let generic_type_name = node.path.segments[0].ident.to_string();
            if self.generic_type_names.contains(&generic_type_name) {
                self.associated_types.entry(generic_type_name).or_insert(vec![]).push(node.clone());
            }
        }

        visit::visit_type_path(self, node);
    }
}

fn get_generic_associated_types(st: &syn::DeriveInput) -> HashMap<String, Vec<syn::TypePath>> {
    let origin_generic_param_names: Vec<_> = st.generics.params.iter().filter_map(|f| {
        if let syn::GenericParam::Type(syn::TypeParam { ref ident, .. }) = f {
            return Some(ident.to_string())
        }
        None
    }).collect();

    let mut visitor = TypePathVisitor {
        generic_type_names: origin_generic_param_names,
        associated_types: HashMap::new(),
    };

    visitor.visit_derive_input(st);

    visitor.associated_types
}

fn get_struct_escape_hatch(st: &syn::DeriveInput) -> Option<String> {
    if let Some(inert_attr) = st.attrs.last() {
        if let Ok(syn::Meta::List(syn::MetaList { nested, .. })) = inert_attr.parse_meta() {
            if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(path_value))) = nested.last() {
                if path_value.path.is_ident("bound") {
                    // eprint!("{:#?}", st.attrs);
                    // eprint!("{:#?}", inert_attr.parse_meta());
                    if let syn::Lit::Str(ref lit) = path_value.lit {
                        return Some(lit.value());
                    }
                }
            }
        }
    }
    None
}