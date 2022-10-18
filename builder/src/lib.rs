use proc_macro::TokenStream;
use quote::{quote};
use syn::{parse_macro_input, Ident};
use syn::{DeriveInput, spanned::Spanned, Field, Token};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = parse_macro_input!(input as DeriveInput);

    match do_expand(&st) {
        Ok(token_stream) => token_stream.into(),
        Err(error) => error.to_compile_error().into(),
    }
}

fn do_expand(st: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name_ident = &st.ident;
    let builder_name_literal = format!("{}Builder", struct_name_ident.to_string());
    let builder_name_ident = syn::Ident::new(&builder_name_literal, st.span());

    let fields = get_struct_fields_name(st)?;
    let builder_struct_fields_def = gen_builder_struct_fields_def(fields)?;
    let builder_struct_fields_init = gen_builder_struct_fields_init(fields)?;
    let builder_impl_setter = gen_builder_impl_setters(fields)?;
    let builder_impl_build = gen_builder_impl_build(fields, struct_name_ident)?;
    // eprintln!("{:#?}", builder_impl_build);

    let ret = quote!(
        pub struct #builder_name_ident {
            #builder_struct_fields_def
        }

        impl #struct_name_ident {
            pub fn builder() -> #builder_name_ident {
                #builder_name_ident {
                    #(#builder_struct_fields_init),*
                }
            }
        }

        impl #builder_name_ident {
            #(#builder_impl_setter)*

            #builder_impl_build
        }
    );

    Ok(ret)
}

fn gen_builder_struct_fields_init(fields: &StructFields) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let init_clauses: syn::Result<Vec<_>> = fields.iter().map(|field| {
        let ident = &field.ident;
        if get_user_specified_ident_for_vec(field)?.is_some() {
            Ok(quote!(#ident: std::vec::Vec::new()))
        } else {
            Ok(quote!(#ident: std::option::Option::None))
        }
    }).collect();

    init_clauses
}

fn gen_builder_struct_fields_def(fields: &StructFields) -> syn::Result<proc_macro2::TokenStream> {
    let ret: syn::Result<Vec<_>> = fields.iter().map(|field| {
        let ident = &field.ident;
        let ty = &field.ty;
        if let Some(inner_ty) = get_generic_inner_type(&field, "Option") {
            Ok(quote!(#ident: std::option::Option<#inner_ty>))
        } else if get_user_specified_ident_for_vec(&field)?.is_some() {
            Ok(quote!(#ident: #ty))
        } else {
            Ok(quote!(#ident: std::option::Option<#ty>))
        }
    }).collect();
    
    let ret = ret?;
    let ret = quote!(
        #(#ret),*
    );

    // let idents: Vec<_> = fields.iter().map(|field| { &field.ident }).collect();
    // let types: Vec<_> = fields.iter().map(|field| { &field.ty }).collect();

    // let ret = quote!(
    //     #(#idents: Option<#types>),*
    // );

    Ok(ret)
}

type StructFields = syn::punctuated::Punctuated<Field, Token![,]>;

fn get_struct_fields_name(st: &DeriveInput) -> syn::Result<&StructFields> {
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

    Err(syn::Error::new(st.span(), "Must define on a Struct, not Enum".to_string()))
}

fn gen_builder_impl_setters(fields: &StructFields) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let setters: Vec<_> = fields.iter().map(|field| {
        let field_ident = &field.ident;
        let field_type = &field.ty;

        if let Some(inner_ty) = get_generic_inner_type(&field, "Option") {
            quote!(
                fn #field_ident(&mut self, #field_ident: #inner_ty) -> &mut Self {
                    self.#field_ident = std::option::Option::Some(#field_ident);
                    self
                }
            )
        } else if let Ok(Some(ref user_specified_ident)) = get_user_specified_ident_for_vec(field) {
            let mut ret = proc_macro2::TokenStream::new();
            let inner_ty = get_generic_inner_type(field, "Vec").ok_or(syn::Error::new(fields.span(), "each field must be specified with Vec field")).unwrap();
            ret.extend(
                quote!(
                    fn #user_specified_ident(&mut self, #user_specified_ident: #inner_ty) -> &mut Self {
                        self.#field_ident.push(#user_specified_ident);
                        self
                    }
                )
            );

            if user_specified_ident != &field_ident.clone().unwrap() {
                ret.extend(
                    quote!(
                        fn #field_ident(&mut self, #field_ident: #field_type) -> &mut Self {
                            self.#field_ident = #field_ident;
                            self
                        }
                    )
                );
            }

            ret
        } else {
            quote!(
                fn #field_ident(&mut self, #field_ident: #field_type) -> &mut Self {
                    self.#field_ident = std::option::Option::Some(#field_ident);
                    self
                }
            )
        }
    }).collect();

    Ok(setters)
}

fn gen_builder_impl_build(fields: &StructFields, struct_name_ident: &Ident) -> syn::Result<proc_macro2::TokenStream> {
    let check_clause: syn::Result<Vec<_>> = fields.iter().map(|field| {
        let ident = &field.ident;

        if get_generic_inner_type(&field, "Option").is_none() && get_user_specified_ident_for_vec(field)?.is_none() {
            Ok(
                quote!(
                    if self.#ident.is_none() {
                        let error_msg = format!("{} can't not be empty!", stringify!(#ident));
                        return std::result::Result::Err(error_msg.into())
                    }
                )
            )
        } else {
            Ok(quote!())
        }
    }).collect();

    let field_clause: syn::Result<Vec<_>> = fields.iter().map(|field| {
        let ident = &field.ident;
        if get_user_specified_ident_for_vec(field)?.is_some() {
            Ok(quote!(#ident: self.#ident.clone()))
        }else if get_generic_inner_type(&field, "Option").is_none() {
            Ok(quote!(#ident: self.#ident.clone().unwrap()))
        } else {
            Ok(quote!(#ident: self.#ident.clone()))
        }
    }).collect();

    let field_clause = field_clause?;
    let ret_clause = quote!(
        let ret = #struct_name_ident {
            #(#field_clause),*
        };

        std::result::Result::Ok(ret)
    );

    let check_clause = check_clause?;
    let ret = quote!(
        pub fn build(&mut self) -> std::result::Result<#struct_name_ident, std::boxed::Box<dyn std::error::Error>> {
            #(#check_clause)*
            #ret_clause
        }
    );

    Ok(ret)
}

fn get_generic_inner_type<'a>(field: &'a syn::Field, outer_ident_name: &'a str) -> Option<&'a syn::Type> {
    let field_type = &field.ty;
    if let syn::Type::Path(syn::TypePath { ref path, .. }) = field_type {
        if let Some(segment) = path.segments.last() {
            if segment.ident == outer_ident_name {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { ref args, .. }) = segment.arguments {
                    if let Some(syn::GenericArgument::Type(inner_ty)) = args.first() {
                        // eprintln!("{:#?}", inner_ty);
                        return Some(inner_ty)
                    }
                }
            }
        }
    } 
    
    None
}

fn get_user_specified_ident_for_vec(field: &syn::Field) -> syn::Result<Option<syn::Ident>> {
    for attr in &field.attrs {
        if let Ok(syn::Meta::List(syn::MetaList{ ref path, ref nested, .. })) = attr.parse_meta() {
            if let Some(p) = path.segments.first() {
                if p.ident == "builder" {
                    if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(kv))) = nested.first() {
                        if kv.path.is_ident("each") {
                            if let syn::Lit::Str(ref ident_str) = kv.lit {
                                return Ok(Some(syn::Ident::new(ident_str.value().as_str(), attr.span())))
                            }
                        } else {
                            if let Ok(syn::Meta::List(ref list)) = attr.parse_meta() {
                                return Err(syn::Error::new_spanned(list, r#"expected `builder(each = "...")`"#))
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(None)
}

// #[proc_macro_derive(ExploreAttribute, attributes(Bar))]
// pub fn attribute_explore(input: TokenStream) -> TokenStream {
//     let st = syn::parse_macro_input!(input as syn::DeriveInput);
//     let attr = st.attrs.first();
//     let meta = attr.parse_meta().unwrap();
//     eprintln!("{:#?}", st);
//     proc_macro2::TokenStream::new().into()
// }

// #[proc_macro_attribute]
// pub fn blog(_input1: TokenStream, _input2: TokenStream) -> TokenStream {
//     TokenStream::new()
// }

// use derive_builder::ExploreAttribute;
// // use derive_builder::blog;

// // #[blog(each="arg")]

// #[derive(ExploreAttribute)]
// pub struct Foo{
//     #[Bar(each="arg")]
//     a: i32
// }

// fn main() {
    
// }