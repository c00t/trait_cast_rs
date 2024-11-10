#![deny(missing_docs)]
#![warn(clippy::undocumented_unsafe_blocks, clippy::pedantic, clippy::nursery)]
//! Proc-macro automating the implementation of `trait_cast_rs::TraitcastableAny`.
//!
//! See `make_trait_castable` for more details.

use proc_macro::TokenStream as TokenStream1;
use proc_macro2::TokenStream;
use quote::quote;
use venial::{parse_declaration, Declaration, Error};
mod unique_id;

/// Attribute macro implementing `TraitcastableAny` for a struct, enum or union.
///
/// Use the arguments to specify all possible target Traits for witch trait objects are
/// supposed to be downcastable from a dyn `TraitcastableAny`.
///
/// Example:
/// ```no_build
///   extern crate trait_cast_rs;
///
///   use trait_cast_rs::{make_trait_castable, TraitcastTarget, TraitcastTo, TraitcastableAny};
///
///
///   #[make_trait_castable(Print)]
///   struct Source(i32);
///
///   trait Print {
///     fn print(&self);
///   }
///   impl Print for Source {
///     fn print(&self) {
///       println!("{}", self.0)
///     }
///   }
///
///   fn main() {
///     let source = Box::new(Source(5));
///     let castable: Box<dyn TraitcastableAny> = source;
///     let x: &dyn Print = castable.downcast_ref().unwrap();
///     x.print();
///   }
/// ```
#[proc_macro_attribute]
pub fn make_trait_castable(args: TokenStream1, input: TokenStream1) -> TokenStream1 {
  let args_stream = TokenStream::from(args);
  // Parse the arguments into traits and version
  let mut args_iter = args_stream.into_iter();
  let mut traits = Vec::new();
  let mut version = None;
  let mut meet_equal = false;
  // Collect traits until we hit a colon
  while let Some(token) = args_iter.next() {
    if token.to_string() == "=" {
      // Parse version tuple after colon
      version = Some(args_iter.collect::<TokenStream>());
      meet_equal = true;
      // version must be the last arg, so just break
      break;
    }
    traits.push(token);
  }

  let traits = TokenStream::from_iter(traits);
  let input = match parse_declaration(input.into()) {
    Ok(Declaration::Function(fun)) => {
      return Error::new_at_span(
        fun.name.span(),
        "Can not implement `Traitcast` for functions, expected a struct, enum or union definition",
      )
      .to_compile_error()
      .into();
    },
    Ok(input) => input,
    Err(error) => {
      return error.to_compile_error().into();
    },
  };
  let item_name = input.name();

  if meet_equal {
    let version = version.unwrap();
    TokenStream1::from(quote!(
        #input
        make_trait_castable_decl_with_version! {
            #item_name => (#traits) : #version
        }
    ))
  } else {
    TokenStream1::from(quote!(
      #input
      make_trait_castable_decl! {
      #item_name => (#traits)
    }))
  }
}

///
#[proc_macro_attribute]
pub fn make_trait_castable_random_self_id(args: TokenStream1, input: TokenStream1) -> TokenStream1 {
  let args = TokenStream::from(args);
  let input = match parse_declaration(input.into()) {
    Ok(Declaration::Function(fun)) => {
      return Error::new_at_span(
        fun.name.span(),
        "Can not implement `Traitcast` for functions, expected a struct, enum or union definition",
      )
      .to_compile_error()
      .into();
    },
    Ok(input) => input,
    Err(error) => {
      return error.to_compile_error().into();
    },
  };
  let item_name = input.name();

  TokenStream1::from(quote!(
    #input
    make_trait_castable_decl_random_self_id! {
    #item_name => (#args)
  }))
}

/// Macro to generate a unique id for a trait object type or a general type.
///
/// This macro generates unique type IDs that can be used for type identification and casting.
/// It supports both trait objects and concrete types, with optional version or file-based uniqueness.
///
/// # Examples
///
/// Basic usage with trait objects:
/// ```ignore,no_compile
/// use trait_cast_rs::unique_id;
///
/// unique_id! {
///     dyn MyTrait
/// }
/// ```
///
/// With version-based uniqueness (commonly used pattern):
/// ```ignore,no_compile
/// use trait_cast_rs::unique_id;
///
/// unique_id! {
///     #[UniqueTypeIdVersion((0,1,0))]
///     dyn my_crate::api::MyTrait
/// }
/// ```
///
/// With file-based uniqueness and filename:
/// ```ignore,no_compile
/// use trait_cast_rs::unique_id;
///
/// unique_id! {
///     #[UniqueTypeIdFile("ids.toml")]
///     dyn MyTrait
/// }
/// ```
///
/// Real-world example from APIs:
/// ```ignore,no_compile
/// use trait_cast_rs::unique_id;
///
/// unique_id! {
///     #[UniqueTypeIdVersion((1,0,0))]
///     dyn bubble_core::api::my_api::MyApi
/// }
/// ```
#[proc_macro]
pub fn unique_id(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  unique_id::unique_id_dyn(input)
}

/// Macro to generate a unique id for a trait object type without version.
///
/// For types which version is managed externally, or want to use semantic versioning.
#[proc_macro]
pub fn unique_id_without_version_hash(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  unique_id::unique_id_dyn_without_version_hash_in_type(input)
}

/// Macro to generate a random unique id.
#[proc_macro]
pub fn random_unique_id(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  unique_id::random_unique_id_dyn(input)
}
