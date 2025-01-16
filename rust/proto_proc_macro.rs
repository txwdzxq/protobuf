google3::import! {
    "//third_party/rust/proc_macro2/v1:proc_macro2";
    "//third_party/rust/quote/v1:quote";
    "//third_party/rust/syn/v2:syn";
}

extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::ParseStream, parse_macro_input, parse_quote, punctuated::Punctuated, Error, Expr,
    ExprArray, ExprPath, ExprStruct, FieldValue, Ident, Member, Path, QSelf, Result, Token, Type,
    TypePath,
};

#[proc_macro]
pub fn proto(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let result = parse_macro_input!(input with parse_and_expand_top_level_struct);
    result.to_token_stream().into()
}

fn parse_and_expand_top_level_struct(input: ParseStream) -> Result<Expr> {
    expand_struct(input.parse()?, EnclosingContext::TopLevel)
}

#[derive(Clone)]
enum EnclosingContext {
    TopLevel,
    Struct { field: Ident },
    Array { repeated_field: Ident },
}

fn expand_struct(
    ExprStruct { attrs, qself, path, fields, rest, .. }: ExprStruct,
    enclosing_context: EnclosingContext,
) -> Result<Expr> {
    if let Some(attr) = attrs.first() {
        return Err(Error::new_spanned(attr, "unsupported syntax"));
    }

    let merge = rest.map(|rest| {
        quote! {
            ::protobuf::MergeFrom::merge_from(&mut this, #rest);
        }
    });

    let fields = expand_struct_fields(fields)?;
    let this_type = expand_struct_type(qself.clone(), path.clone(), enclosing_context.clone());
    let (head, tail) =
        expand_struct_head_tail(qself.clone(), path.clone(), enclosing_context.clone())?;

    Ok(parse_quote! {{
        let mut this: #this_type = #head;
        #merge
        #fields
        #tail
    }})
}

fn expand_struct_fields(fields: Punctuated<FieldValue, Token![,]>) -> Result<TokenStream> {
    fields
        .into_iter()
        .map(|field| {
            let Member::Named(ident) = field.member else {
                return Err(Error::new_spanned(field, "field must be an identifier, not an index"));
            };
            let set_method = format_ident!("set_{}", ident);
            let enclosing_context = EnclosingContext::Struct { field: ident };
            let expr = match field.expr {
                Expr::Struct(struct_) => {
                    // In a nested message, the value will be mutated in-place, so there is no need
                    // to call the top-level setter.
                    return Ok(expand_struct(struct_, enclosing_context)?.to_token_stream());
                }
                Expr::Array(array) => expand_array(array, enclosing_context)?,
                expr => expr,
            };
            Ok(quote! {
                this.#set_method(#expr);
            })
        })
        .collect()
}

fn expand_struct_type(
    qself: Option<QSelf>,
    path: Path,
    enclosing_context: EnclosingContext,
) -> Type {
    let type_path = TypePath { qself: qself.clone(), path: path.clone() };
    if should_infer_message_type(qself, path) {
        parse_quote!(_)
    } else if let EnclosingContext::Struct { .. } = enclosing_context {
        // Nested messages use a mutable view type.
        parse_quote!(::protobuf::Mut<'_, #type_path>)
    } else {
        parse_quote!(#type_path)
    }
}

fn expand_struct_head_tail(
    qself: Option<QSelf>,
    path: Path,
    enclosing_context: EnclosingContext,
) -> Result<(Expr, Expr)> {
    match enclosing_context {
        EnclosingContext::TopLevel => {
            if should_infer_message_type(qself.clone(), path.clone()) {
                Err(Error::new_spanned(path, "message type must be specified explicitly"))
            } else {
                let path = ExprPath { attrs: Vec::new(), qself, path };
                Ok((parse_quote!(#path::new()), parse_quote!(this)))
            }
        }
        EnclosingContext::Struct { field } => {
            // In a nested message, mutate the field in-place.
            let field_mut = format_ident!("{}_mut", field);
            Ok((parse_quote!(this.#field_mut()), parse_quote!(())))
        }
        EnclosingContext::Array { repeated_field } => {
            // In a message inside an array, create a new message and return it.
            Ok((
                parse_quote!(::protobuf::__internal::get_repeated_default_value(
                    ::protobuf::__internal::Private,
                    this.#repeated_field()
                )),
                parse_quote!(this),
            ))
        }
    }
}

fn should_infer_message_type(qself: Option<QSelf>, path: Path) -> bool {
    qself.is_none() && path.get_ident().is_some_and(|ident| *ident == "__")
}

fn expand_array(array: ExprArray, enclosing_context: EnclosingContext) -> Result<Expr> {
    if let Some(attr) = array.attrs.first() {
        return Err(Error::new_spanned(attr, "unsupported syntax"));
    }

    let enclosing_context = EnclosingContext::Array {
        repeated_field: match enclosing_context {
            EnclosingContext::Struct { field } => field,
            EnclosingContext::TopLevel | EnclosingContext::Array { .. } => {
                return Err(Error::new_spanned(array, "arrays must be nested inside a message"))
            }
        },
    };

    let array = array
        .elems
        .into_iter()
        .map(|elem| match elem {
            Expr::Struct(struct_) => expand_struct(struct_, enclosing_context.clone()),
            Expr::Array(array) => expand_array(array, enclosing_context.clone()),
            expr => Ok(expr),
        })
        .collect::<Result<Vec<Expr>>>()?;

    Ok(parse_quote!([#(#array),*].into_iter()))
}
