use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Ident, LitStr, Result};

#[proc_macro_derive(Relation, attributes(multi, cleanup))]
pub fn relation_derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let ident = &ast.ident;
    let exclusive = !ast.attrs.iter().any(|attr| attr.path().is_ident("multi"));

    let cleanup = match parse_cleanup_attr(&ast) {
        Ok(cleanup) => cleanup,
        Err(e) => return e.into_compile_error().into(),
    };

    let output = quote! {
        impl Relation for #ident {
            const CLEANUP_POLICY: CleanupPolicy = CleanupPolicy::#cleanup;
            const EXCLUSIVE: bool = #exclusive;
        }
    };

    output.into()
}

fn parse_cleanup_attr(ast: &DeriveInput) -> Result<TokenStream2> {
    let mut policy = String::from("Orphan");

    for meta in ast
        .attrs
        .iter()
        .filter(|attr| attr.path().is_ident("cleanup"))
    {
        meta.parse_nested_meta(|nested| {
            if nested.path.is_ident("policy") {
                policy = match nested.value()?.parse::<LitStr>()?.value().as_str() {
                    ident @ ("Orphan" | "Counted" | "Recursive" | "Total") => ident.to_owned(),
                    invalid => {
                        return Err(nested.error(format!(
                            "Unrecognized cleanup policy `{invalid}`. Available policies are:\n\
                            - Orphan\n\
                            - Counted\n\
                            - Recursive\n\
                            - Total\n"
                        )))
                    }
                };
                Ok(())
            } else {
                Err(nested.error("Unrecognized attribute"))
            }
        })?;
    }

    let policy = Ident::new(&policy, Span::call_site());

    Ok(quote! { #policy })
}
