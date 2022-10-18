use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{visit_mut::{self, VisitMut}};

#[proc_macro_attribute]
pub fn sorted(_args: TokenStream, input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::Item);
    
    match do_expend(&st) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => {
            let mut t = e.to_compile_error();
            t.extend(st.to_token_stream());
            t.into()
        },
    }
}

fn do_expend(st: &syn::Item) -> syn::Result<proc_macro2::TokenStream> {
    let ret = match st {
        syn::Item::Enum(enum_node) => check_enum_order(enum_node),
        _ => Err(syn::Error::new(proc_macro2::Span::call_site(), "expected enum or match expression")),
    }?;

    Ok(ret)
}

fn check_enum_order(enum_node: &syn::ItemEnum) -> syn::Result<proc_macro2::TokenStream> {
    let origin_names: Vec<_> = enum_node.variants.iter().map(|item| (&item.ident, item)).collect();
    let mut sorted_origin_names = origin_names.clone();
    sorted_origin_names.sort_by(|a, b| a.0.cmp(b.0));

    for (a, b) in origin_names.iter().zip(sorted_origin_names.iter()) {
        if a.0 != b.0 {
            return Err(syn::Error::new(b.1.ident.span(), format!("{} should sort before {}", b.0, a.0)));
        }
    }

    Ok(enum_node.to_token_stream())
}

#[proc_macro_attribute]
pub fn check(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut st = syn::parse_macro_input!(input as syn::ItemFn);
    
    match do_match_expand(&mut st) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => {
            let mut t = e.to_compile_error();
            t.extend(st.to_token_stream());
            t.into()
        },
    }
}

struct MatchVisitor {
    err: Option<syn::Error>,
}

fn do_match_expand(st: &mut syn::ItemFn) -> syn::Result<proc_macro2::TokenStream> {
    let mut visitor = MatchVisitor{err:None};

    visitor.visit_item_fn_mut(st);
    
    if visitor.err.is_none() {
        return syn::Result::Ok( st.to_token_stream())
    } else {
        return syn::Result::Err(visitor.err.unwrap());
    }
}

fn get_path_string(p: &syn::Path) -> String {
    let mut v = Vec::new();
    for s in &p.segments {
        v.push(s.ident.to_string());
    }

    v.join("::")
}

impl syn::visit_mut::VisitMut for MatchVisitor {
    fn visit_expr_match_mut(&mut self, i: &mut syn::ExprMatch) {
        let mut target_idx:isize = -1;

        for (idx, attr) in i.attrs.iter().enumerate() {
            if get_path_string(&attr.path) == "sorted" {
                target_idx = idx as isize;
                break;
            }
        }

        if target_idx != -1 {
            i.attrs.remove(target_idx as usize);

            let mut match_arm_names: Vec<(String, &dyn ToTokens)> = Vec::new();
            for arm in &i.arms {
                match &arm.pat {
                    syn::Pat::Path(p) => {
                        match_arm_names.push((get_path_string(&p.path),&p.path))
                    },syn::Pat::TupleStruct(p) => {
                        match_arm_names.push((get_path_string(&p.path), &p.path));
                    },
                    syn::Pat::Struct(p) => {
                        match_arm_names.push((get_path_string(&p.path), &p.path));
                    },
                    syn::Pat::Ident(p) => {
                        match_arm_names.push((p.ident.to_string(), &p.ident));
                    },
                    syn::Pat::Wild(p) => {
                        match_arm_names.push(("_".to_string(), &p.underscore_token));
                    },
                    _ => {
                        // self.err = Some(syn::Error::new(arm.pat.span(), "unsupported by #[sorted]"));
                        self.err = Some(syn::Error::new_spanned(&arm.pat, "unsupported by #[sorted]"));
                        return
                    }
                }
            }

            let mut sorted_names = match_arm_names.clone();
            sorted_names.sort_by(|a,b|{a.0.cmp(&b.0)});
            for (a,b) in match_arm_names.iter().zip(sorted_names.iter()) {
                if a.0 != b.0 {
                    self.err = Some(syn::Error::new_spanned(b.1, format!("{} should sort before {}", b.0, a.0)));
                    return
                }
            }
        } 

        visit_mut::visit_expr_match_mut(self, i)
    }
}