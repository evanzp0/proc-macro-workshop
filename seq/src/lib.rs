#![allow(unused_variables, dead_code, unused_macros)]

use proc_macro::TokenStream;
use quote::ToTokens;

#[derive(Debug)]
struct SeqParser {
    variable_ident: syn::Ident,
    start: isize,
    end: isize,
    body: proc_macro2::TokenStream,
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as SeqParser);
    
    let buffer = syn::buffer::TokenBuffer::new2(st.body.clone());
    let(ret_1, expanded) =  st.find_block_to_expand_and_do_expand(buffer.begin());
    if expanded {
        return ret_1.into()
    }

    let mut ret = proc_macro2::TokenStream::new();
    for i in st.start..st.end {
        ret.extend(st.expand(&st.body, i))
    }
    return ret.into()
}

impl syn::parse::Parse for SeqParser {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let variable_ident: syn::Ident = input.parse()?;    

        input.parse::<syn::Token!(in)>()?;

        let start: syn::LitInt = input.parse()?;

        input.parse::<syn::Token!(..)>()?;
        let mut inc = false;
        if input.peek(syn::Token!(=)) {
            input.parse::<syn::Token!(=)>()?;
            inc =true;
        }

        let end: syn::LitInt = input.parse()?;

        let body_buf;
        syn::braced!(body_buf in input);
        let body: proc_macro2::TokenStream  = body_buf.parse()?;

        let mut t = SeqParser {
            variable_ident,
            start: start.base10_parse()?,
            end: end.base10_parse()?,
            body,
        };

        if inc {
            t.end += 1;
        }

        return Ok(t);
    }
}

impl SeqParser {
    fn expand(&self, ts: &proc_macro2::TokenStream, n: isize) -> proc_macro2::TokenStream {
        let buf = ts.clone().into_iter().collect::<Vec<_>>();
        let mut ret = proc_macro2::TokenStream::new();
        
        let mut idx = 0;
        while idx < buf.len() {
            let tree_node = &buf[idx];
            match tree_node {
                proc_macro2::TokenTree::Group(g) => {
                    let new_stream = self.expand(&g.stream(), n);
                    let new_stream = proc_macro2::Group::new(g.delimiter(), new_stream);
                    ret.extend(new_stream.to_token_stream());
                }
                proc_macro2::TokenTree::Ident(prefix) => {
                    if idx + 2 < buf.len() {
                        if let proc_macro2::TokenTree::Punct(p) = &buf[idx + 1] {
                            if p.as_char() == '~' {
                                if let proc_macro2::TokenTree::Ident(i) = &buf[idx + 2] {
                                    if i == &self.variable_ident 
                                    && prefix.span().end() == p.span().start()
                                    && p.span().end() == i.span().start()
                                    {
                                        let new_ident_litral = format!("{}{}", prefix.to_string(), n);
                                        let new_ident = proc_macro2::Ident::new(&new_ident_litral, prefix.span());
                                        ret.extend(new_ident.to_token_stream());
                                        idx += 3;
                                        continue;
                                    }
                                }
                            }
                        }
                    }

                    if prefix == &self.variable_ident {
                        let new_ident = proc_macro2::Literal::i64_unsuffixed(n as i64);
                        ret.extend(new_ident.to_token_stream());
                        idx += 1;
                        continue;
                    } else {
                        ret.extend(tree_node.to_token_stream());
                    }
                }
                _ => {
                    ret.extend(tree_node.to_token_stream());
                }
            }
            idx+=1;
        }
        ret
    } 

    
    fn find_block_to_expand_and_do_expand(&self, c: syn::buffer::Cursor) -> (proc_macro2::TokenStream, bool) {
        let mut found = false;
        let mut ret = proc_macro2::TokenStream::new();

        let mut cursor = c;
        while !cursor.eof() {
            if let Some((punct_prefix, cursor_1)) = cursor.punct() {
                if punct_prefix.as_char() == '#' {
                    if let Some((group_cur, _, cursor_2)) = cursor_1.group(proc_macro2::Delimiter::Parenthesis) {
                        if let Some((punct_suffix, cursor_3)) = cursor_2.punct() {
                            if punct_suffix.as_char() == '*' {
                                for i in self.start..self.end {
                                    let t = self.expand(&group_cur.token_stream(), i);
                                    ret.extend(t);
                                }
                                cursor = cursor_3;
                                found = true;
                                continue;
                            }
                        }
                    }
                }
            }

            if let Some((group_cur, _, next_cur)) = cursor.group(proc_macro2::Delimiter::Brace) {
                let (t, f) = self.find_block_to_expand_and_do_expand(group_cur);
                found = f;
                ret.extend(quote::quote!({#t}));
                cursor = next_cur;
                continue
            } else if let Some((group_cur,_, next_cur)) = cursor.group(proc_macro2::Delimiter::Bracket) {
                let (t, f) = self.find_block_to_expand_and_do_expand(group_cur);
                found = f;
                ret.extend(quote::quote!([#t]));
                cursor = next_cur;
                continue
            } else if let Some((group_cur,_, next_cur)) = cursor.group(proc_macro2::Delimiter::Parenthesis) {
                let (t, f) = self.find_block_to_expand_and_do_expand(group_cur);
                found = f;
                ret.extend(quote::quote!((#t)));
                cursor = next_cur;
                continue
            } else if let Some((punct ,next_cur)) = cursor.punct() {
                ret.extend(quote::quote!(#punct));
                cursor = next_cur;
                continue
            } else if let Some((ident ,next_cur)) = cursor.ident() {
                ret.extend(quote::quote!(#ident));
                cursor = next_cur;
                continue
            } else if let Some((literal ,next_cur)) = cursor.literal() {
                ret.extend(quote::quote!(#literal));
                cursor = next_cur;
                continue
            } else if let Some((lifetime ,next_cur)) = cursor.lifetime() {
                // lifetime这种特殊的分类也是用cursor模式来处理的时候特有的，之前`proc_macro2::TokenTree`里面没有定义这个分类
                ret.extend(quote::quote!(#lifetime));
                cursor = next_cur;
                continue
            }
        }

        (ret, found)

    }
}
