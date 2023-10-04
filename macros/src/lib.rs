use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, punctuated::Punctuated, DeriveInput, Error, Ident, Meta,
    Result, Token,
};

struct RelationConfig {
    policy: Ident,
    exclusive: bool,
    symmetric: bool,
}

fn parse_config(ast: &DeriveInput) -> Result<RelationConfig> {
    let mut policy = "Orphan";
    let mut exclusive = true;
    let mut symmetric = false;

    for attr in ast.attrs.iter().filter(|attr| attr.path().is_ident("aery")) {
        let nested = attr.parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)?;
        for meta in nested {
            match meta {
                Meta::Path(ref path) => {
                    if let Some(new_policy) = ["Counted", "Recursive", "Total"]
                        .into_iter()
                        .find(|ident| path.is_ident(ident))
                    {
                        if policy != "Orphan" {
                            return Err(Error::new_spanned(
                                meta,
                                "Tried to set policy multiple times",
                            ));
                        }
                        policy = new_policy;
                    } else if path.is_ident("Poly") {
                        if !exclusive {
                            return Err(Error::new_spanned(
                                meta,
                                "Tried to set exclusivity multiple times",
                            ));
                        }
                        exclusive = false;
                    } else if path.is_ident("Symmetric") {
                        if symmetric {
                            return Err(Error::new_spanned(
                                meta,
                                "Tried to set symmetry multiple times",
                            ));
                        }
                        symmetric = true;
                    } else {
                        return Err(Error::new_spanned(meta, "Unrecognized property override"));
                    }
                }
                _ => {
                    return Err(Error::new_spanned(meta, "Unrecognized macro format"));
                }
            }
        }
    }

    Ok(RelationConfig {
        policy: Ident::new(policy, Span::call_site()),
        exclusive,
        symmetric,
    })
}

#[proc_macro_derive(Relation, attributes(aery))]
pub fn relation_derive(input: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(input as DeriveInput);

    let RelationConfig {
        policy,
        exclusive,
        symmetric,
    } = match parse_config(&ast) {
        Ok(config) => config,
        Err(e) => return e.into_compile_error().into(),
    };

    ast.generics
        .make_where_clause()
        .predicates
        .push(parse_quote! { Self: Sized + Send + Sync + 'static });

    let struct_name = &ast.ident;
    let (impl_generics, type_generics, where_clause) = &ast.generics.split_for_impl();

    let output = quote! {
        impl #impl_generics Relation for #struct_name #type_generics #where_clause  {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::#policy;
            const EXCLUSIVE: bool = #exclusive;
            const SYMMETRIC: bool = #symmetric;
        }
    };

    output.into()
}
