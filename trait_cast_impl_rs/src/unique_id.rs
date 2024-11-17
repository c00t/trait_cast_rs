use core::panic;
use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use rand::prelude::*;
use std::{fs::File, mem::discriminant};
use syn::{
  parse::{Parse, ParseStream},
  parse_macro_input,
  punctuated::Punctuated,
  Attribute, Expr, GenericArgument, Lit, Meta, MetaNameValue, PathArguments, Result, Token,
};

/// Custom structure to represent `dyn TraitName` and multiple attributes
struct DynTraitInput {
  file: Option<String>, // The value of UniqueTypeIdFile, e.g. "types.toml"
  version: (u64, u64, u64),
  type_id_equal_to: Option<String>,
  paths: Vec<syn::Path>,
  /// current generics is always empty, because that is parsed to [`Self::paths`]
  generics: Vec<Vec<GenericArgument>>,
  is_dyn: Vec<bool>,
}

impl Parse for DynTraitInput {
  fn parse(input: ParseStream) -> Result<Self> {
    let mut file = None;
    // Parse the outer attributes (e.g., #[UniqueTypeIdFile], #[UniqueTypeIdStart], etc.)
    let attrs: Vec<Attribute> = input.call(Attribute::parse_outer)?;
    let mut version = (0, 0, 0);
    let mut type_id_equal_to = None;
    for attr in attrs {
      if attr.path().is_ident("UniqueTypeIdFile") {
        if let Expr::Lit(expr_lit) = attr.parse_args()? {
          if let Lit::Str(lit_str) = expr_lit.lit {
            file = Some(lit_str.value());
          }
        }
      } else if attr.path().is_ident("UniqueTypeIdVersion") {
        if let Expr::Tuple(expr_tuple) = attr.parse_args()? {
          let mut version_parts = vec![];
          for elem in expr_tuple.elems {
            if let Expr::Lit(expr_lit) = elem {
              if let Lit::Int(lit_int) = expr_lit.lit {
                version_parts.push(lit_int.base10_parse::<u64>().unwrap());
              }
            } else if let Expr::Group(expr_group) = elem {
              // used inside declare macro
              let inner = expr_group.expr;
              if let Expr::Lit(expr_lit) = *inner {
                if let Lit::Int(lit_int) = expr_lit.lit {
                  version_parts.push(lit_int.base10_parse::<u64>().unwrap());
                }
              }
            }
          }
          assert_eq!(version_parts.len(), 3);
          version = (version_parts[0], version_parts[1], version_parts[2]);
        }
      } else if attr.path().is_ident("TypeIdEqualTo") {
        // all types scope in this macro will have the same TypeIdEqualTo type specified by this attribute
        if let Expr::Lit(expr_lit) = attr.parse_args()? {
          if let Lit::Str(lit_str) = expr_lit.lit {
            type_id_equal_to = Some(lit_str.value());
          }
        }
      }
      // else if attr.path().is_ident("UniqueTypeIdType") {
      //     if let Expr::Lit(expr_lit) = value {
      //         if let Lit::Str(lit_str) = expr_lit.lit {
      //             type_id = Some(lit_str.value());
      //         }
      //     }
      // }
    }
    // Parse the `dyn TraitName` part with following format:
    //
    // dyn TraitName
    // dyn TraitName<T,U,K<T>>
    // StructName
    // StructName<T,U,K<T>>

    let mut paths = Vec::new();
    let mut generics = Vec::new();
    let mut is_dyn = Vec::new();
    // Parse multiple definitions
    while !input.is_empty() {
      // Check if it's a dyn trait
      let is_dyn_current = input.parse::<Token![dyn]>().is_ok();
      is_dyn.push(is_dyn_current);

      // then is a path
      let path: syn::Path = input.parse()?;

      // Optionally parse "::" before generic arguments, when parsed by `syn``
      if input.peek(Token![::]) {
        let _ = input.parse::<Token![::]>();
      }
      // Parse the generics
      let current_generics = path
        .segments
        .last()
        .and_then(|seg| match &seg.arguments {
          PathArguments::AngleBracketed(angle_bracketed) => {
            Some(angle_bracketed.args.clone().into_iter().collect())
          },
          _ => None,
        })
        .unwrap_or(Vec::new());
      paths.push(path);
      generics.push(current_generics);

      // If there's a comma, continue to the next definition
      if input.parse::<Token![;]>().is_ok() {
        continue;
      } else {
        break;
      }
    }

    Ok(DynTraitInput {
      file,
      version,
      type_id_equal_to,
      paths,
      generics,
      is_dyn,
    })
  }
}

fn store_id_in_file(file_name: &str, type_names: &[String], idvec: &[u64]) -> std::io::Result<()> {
  use std::io::Write;
  let path = std::path::Path::new(file_name);
  let mut ids = std::collections::HashMap::new();

  // Read existing IDs from the file
  if path.exists() {
    let file = File::open(path)?;
    let reader = std::io::BufReader::new(file);
    for line in std::io::BufRead::lines(reader) {
      let line = line?;
      let parts: Vec<&str> = line.split('=').collect();
      if parts.len() == 2 {
        ids.insert(
          parts[0].trim().to_string(),
          parts[1].trim().parse::<u64>().unwrap(),
        );
      }
    }
    for (type_name, id) in type_names.iter().zip(idvec.iter()) {
      // Update or add the new ID
      let entry = ids.entry(type_name.to_string()).or_insert(*id);
      *entry = *id;
    }

    // Write all IDs back to the file
    let mut file = std::fs::OpenOptions::new()
      .write(true)
      .create(true)
      .truncate(true)
      .open(path)?;

    for (name, id) in ids {
      writeln!(file, "{} = {}", name, id)?;
    }
    Ok(())
  } else {
    panic!("File {} doesn't exist!", file_name);
  }
}

fn path_to_prefix_path(path: &syn::Path) -> (syn::Path, syn::Path) {
  // the last segments is the name
  // other segments are the prefix
  let mut prefix = path.clone();
  let mut name = path.clone();
  prefix.segments.pop();
  name.segments = name
    .segments
    .last()
    .map(|seg| {
      let mut new_path = syn::Path::from(seg.ident.clone());
      new_path.segments[0].arguments = seg.arguments.clone();
      new_path.segments
    })
    .unwrap_or_default();
  (prefix, name)
}

fn path_to_string(path: &syn::Path) -> String {
  format!("{}", quote!(#path))
}

pub fn unique_id_dyn(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let ast = parse_macro_input!(input as DynTraitInput);
  let types_file_name = ast.file;
  let id_type = syn::parse_str::<syn::Type>("u64").unwrap();

  let mut implementations = Vec::new();

  let mut names = vec![];
  let mut hashes = vec![];

  for (index, path) in ast.paths.iter().enumerate() {
    let path_str = path_to_string(path);
    let (_prefix_path, path) = path_to_prefix_path(path);
    // Hash the name and version to a u64
    names.push(path_str.clone());
    let mut hasher = std::hash::DefaultHasher::new();
    let is_dyn = if ast.is_dyn[index] {
      quote! { dyn }
    } else {
      quote! {}
    };
    std::hash::Hash::hash(&is_dyn.to_string(), &mut hasher);
    // path string
    std::hash::Hash::hash(&path_str, &mut hasher);
    // generics
    // let generics = if !ast.generics[index].is_empty() {
    //   let gen_params: Vec<_> = ast.generics[index].iter().map(|g| quote! { #g }).collect();
    //   quote! { <#(#gen_params),*> }
    // } else {
    //   quote! {}
    // };
    // let generic_str = format!("{}", generics);
    // std::hash::Hash::hash(&generic_str, &mut hasher);

    // version
    std::hash::Hash::hash(&ast.version, &mut hasher);
    let mut hash = std::hash::Hasher::finish(&hasher);
    if let Some(_) = ast.type_id_equal_to {
      // store 0u64
      hash = 0;
    }
    hashes.push(hash);

    let major = ast.version.0;
    let minor = ast.version.1;
    let patch = ast.version.2;

    let implementation = if let Some(type_id_equal_to) = &ast.type_id_equal_to {
      // create a ident
      let type_id_equal_to_ident =
        Ident::new(type_id_equal_to, path.segments.last().unwrap().ident.span());
      quote! {
        impl self::UniqueTypeId for #is_dyn #path {
            const TYPE_NAME: &'static str = #path_str;
            const TYPE_ID: self::UniqueId = <#type_id_equal_to_ident as self::UniqueTypeId>::TYPE_ID;
            const TYPE_VERSION: (u64, u64, u64) = (#major, #minor, #patch);


            fn ty_name() -> &'static str {
                Self::TYPE_NAME
            }

            fn ty_id() -> self::UniqueId {
                Self::TYPE_ID
            }

            fn ty_version() -> (u64, u64, u64) {
                Self::TYPE_VERSION
            }
        }
      }
    } else {
      quote! {
          impl self::UniqueTypeId for #is_dyn #path {
              const TYPE_NAME: &'static str = #path_str;
              const TYPE_ID: self::UniqueId = self::UniqueId(#hash as #id_type);
              const TYPE_VERSION: (u64, u64, u64) = (#major, #minor, #patch);


              fn ty_name() -> &'static str {
                  Self::TYPE_NAME
              }

              fn ty_id() -> self::UniqueId {
                  Self::TYPE_ID
              }

              fn ty_version() -> (u64, u64, u64) {
                  Self::TYPE_VERSION
              }
          }
      }
    };

    implementations.push(implementation);
  }
  if let Some(file_name) = types_file_name {
    if let Err(e) = store_id_in_file(&file_name, &names, &hashes) {
      panic!("Failed to store ID in file: {}", e);
    }
  }
  // let id = gen_id(&types_file_name, &ast.trait_name.to_string(), gen_start);

  let result = TokenStream::from(quote! {
      #(#implementations)*
  });
  result
}

pub fn unique_id_dyn_without_version_hash_in_type(
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  let ast = parse_macro_input!(input as DynTraitInput);

  let types_file_name = ast.file;
  let id_type = syn::parse_str::<syn::Type>("u64").unwrap();

  let mut implementations = Vec::new();

  let mut names = vec![];
  let mut hashes = vec![];

  for (index, path) in ast.paths.iter().enumerate() {
    let path_str = path_to_string(path);
    let (_prefix_path, path) = path_to_prefix_path(path);
    // Hash the name and version to a u64
    names.push(path_str.clone());
    let mut hasher = std::hash::DefaultHasher::new();
    let is_dyn = if ast.is_dyn[index] {
      quote! { dyn }
    } else {
      quote! {}
    };
    std::hash::Hash::hash(&is_dyn.to_string(), &mut hasher);
    std::hash::Hash::hash(&path_str, &mut hasher);
    // let generics = if !ast.generics[index].is_empty() {
    //   let gen_params: Vec<_> = ast.generics[index].iter().map(|g| quote! { #g }).collect();
    //   quote! { <#(#gen_params),*> }
    // } else {
    //   quote! {}
    // };
    // std::hash::Hash::hash(&generics.to_string(), &mut hasher);
    // std::hash::Hash::hash(&ast.version, &mut hasher);
    let mut hash = std::hash::Hasher::finish(&hasher);
    if let Some(_) = ast.type_id_equal_to {
      // store 0u64
      hash = 0;
    }
    hashes.push(hash);

    let major = ast.version.0;
    let minor = ast.version.1;
    let patch = ast.version.2;
    let implementation = if let Some(type_id_equal_to) = &ast.type_id_equal_to {
      // create a ident
      let type_id_equal_to_ident =
        Ident::new(type_id_equal_to, path.segments.last().unwrap().ident.span());
      quote! {
        impl self::UniqueTypeId for #is_dyn #path {
            const TYPE_NAME: &'static str = #path_str;
            const TYPE_ID: self::UniqueId = <#type_id_equal_to_ident as self::UniqueTypeId>::TYPE_ID;
            const TYPE_VERSION: (u64, u64, u64) = (#major, #minor, #patch);

            fn ty_name() -> &'static str {
                Self::TYPE_NAME
            }

            fn ty_id() -> self::UniqueId {
                Self::TYPE_ID
            }

            fn ty_version() -> (u64, u64, u64) {
                Self::TYPE_VERSION
            }
        }
      }
    } else {
      quote! {
          impl self::UniqueTypeId for #is_dyn #path {
              const TYPE_NAME: &'static str = #path_str;
              const TYPE_ID: self::UniqueId = self::UniqueId(#hash as #id_type);
              const TYPE_VERSION: (u64, u64, u64) = (#major, #minor, #patch);
              fn ty_name() -> &'static str {
                  Self::TYPE_NAME
              }
              fn ty_id() -> self::UniqueId {
                  Self::TYPE_ID
              }
              fn ty_version() -> (u64, u64, u64) {
                  Self::TYPE_VERSION
              }
          }
      }
    };

    implementations.push(implementation);
  }

  if let Some(file_name) = types_file_name {
    if let Err(e) = store_id_in_file(&file_name, &names, &hashes) {
      panic!("Failed to store ID in file: {}", e);
    }
  }
  // let id = gen_id(&types_file_name, &ast.trait_name.to_string(), gen_start);

  let result = TokenStream::from(quote! {
      #(#implementations)*
  });

  result
}

pub fn random_unique_id_dyn(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let ast = parse_macro_input!(input as DynTraitInput);

  let types_file_name = ast.file;
  let id_type = syn::parse_str::<syn::Type>("u64").unwrap();

  let mut implementations = Vec::new();

  let mut names = vec![];
  let mut hashes = vec![];

  for (index, path) in ast.paths.iter().enumerate() {
    let path_str = path_to_string(path);
    let (_prefix_path, path) = path_to_prefix_path(path);
    // Hash the name and version to a u64
    names.push(path_str.clone());
    // let mut hasher = std::hash::DefaultHasher::new();
    let is_dyn = if ast.is_dyn[index] {
      quote! { dyn }
    } else {
      quote! {}
    };
    // std::hash::Hash::hash(&is_dyn.to_string(), &mut hasher);
    // std::hash::Hash::hash(&path_str, &mut hasher);
    // let generics = if !ast.generics[index].is_empty() {
    //   let gen_params: Vec<_> = ast.generics[index].iter().map(|g| quote! { #g }).collect();
    //   quote! { <#(#gen_params),*> }
    // } else {
    //   quote! {}
    // };
    // std::hash::Hash::hash(&generics.to_string(), &mut hasher);
    // // std::hash::Hash::hash(&ast.version, &mut hasher);
    // let hash = std::hash::Hasher::finish(&hasher);
    let hash: u64 = random();
    hashes.push(hash);

    let major = ast.version.0;
    let minor = ast.version.1;
    let patch = ast.version.2;
    let implementation = quote! {
        impl self::UniqueTypeId for #is_dyn #path {
            const TYPE_NAME: &'static str = #path_str;
            const TYPE_ID: self::UniqueId = self::UniqueId(#hash as #id_type);
            const TYPE_VERSION: (u64, u64, u64) = (#major, #minor, #patch);
            fn ty_name() -> &'static str {
                Self::TYPE_NAME
            }
            fn ty_id() -> self::UniqueId {
                Self::TYPE_ID
            }
            fn ty_version() -> (u64, u64, u64) {
                Self::TYPE_VERSION
            }
        }
    };

    implementations.push(implementation);
  }

  if let Some(file_name) = types_file_name {
    if let Err(e) = store_id_in_file(&file_name, &names, &hashes) {
      panic!("Failed to store ID in file: {}", e);
    }
  }
  // let id = gen_id(&types_file_name, &ast.trait_name.to_string(), gen_start);

  let result = TokenStream::from(quote! {
      #(#implementations)*
  });

  result
}
