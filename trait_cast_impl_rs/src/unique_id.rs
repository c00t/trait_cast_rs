// Custom structure to represent `dyn TraitName` and multiple attributes
struct DynTraitInput {
  file: Option<String>, // The value of UniqueTypeIdFile, e.g. "types.toml"
  version: (u32, u32, u32),
  // type_id: Option<String>,
  paths: Vec<syn::Path>,
  generics: Vec<Vec<GenericArgument>>,
  is_dyn: Vec<bool>,
}

use core::panic;
use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use std::fs::File;
use syn::{
  parse::{Parse, ParseStream},
  parse_macro_input, Attribute, Expr, GenericArgument, Lit, Meta, MetaNameValue, Result, Token,
};
impl Parse for DynTraitInput {
  fn parse(input: ParseStream) -> Result<Self> {
    let mut file = None;

    // Parse the outer attributes (e.g., #[UniqueTypeIdFile], #[UniqueTypeIdStart], etc.)
    let attrs: Vec<Attribute> = input.call(Attribute::parse_outer)?;
    let mut version = (0, 0, 0);
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
                version_parts.push(lit_int.base10_parse::<u32>().unwrap());
              }
            }
          }
          version = (version_parts[0], version_parts[1], version_parts[2]);
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
      paths.push(path);

      // Optionally parse "::" before generic arguments, when parsed by `syn``
      let _ = input.parse::<Token![::]>();

      // Parse the generics
      let current_generics = if input.peek(syn::token::Lt) {
        let angle_bracketed = input.parse::<syn::AngleBracketedGenericArguments>()?;
        angle_bracketed.args.into_iter().collect()
      } else {
        Vec::new()
      };
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
  path
    .segments
    .iter()
    .map(|seg| seg.ident.to_string())
    .collect::<Vec<_>>()
    .join("::")
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
    // panic!("{path_str}");
    // Hash the name and version to a u64
    names.push(path_str.clone());
    let mut hasher = std::hash::DefaultHasher::new();
    std::hash::Hash::hash(&path_str, &mut hasher);
    std::hash::Hash::hash(&ast.version, &mut hasher);
    let hash = std::hash::Hasher::finish(&hasher);
    hashes.push(hash);

    let generics = if !ast.generics[index].is_empty() {
      let gen_params: Vec<_> = ast.generics[index].iter().map(|g| quote! { #g }).collect();
      quote! { <#(#gen_params),*> }
    } else {
      quote! {}
    };

    let is_dyn = if ast.is_dyn[index] {
      quote! { dyn }
    } else {
      quote! {}
    };

    let implementation = quote! {
        impl self::UniqueTypeId for #is_dyn #path #generics {
            const TYPE_ID: self::UniqueId = self::UniqueId(#hash as #id_type);
            fn id() -> self::UniqueId {
                Self::TYPE_ID
            }
        }
    };

    implementations.push(implementation);
  }
  // eprintln!("{:?}", types_file_name);
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
