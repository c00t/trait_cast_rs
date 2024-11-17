//! A unique id generator for rust types.
//!
//! The crate provides a trait and a procedural macro. By deriving one, you implement the
//! trait with `fn id() -> TypeId` static method. The type id is unique in the whole project.
//!
//! # Usage
//!
//! The example usage:
//!
//! ```rust
//!fn check_id() {
//!    use trait_cast_rs::{UniqueTypeId, UniqueId, unique_id};
//!    
//!    // There is a special macro used for types which isn't a struct.
//!    // It will hash the **type name**(eg. Q in example below) and version(a (u32,u32,u32) tuple, eg. (0,1,0)) to generate a unique id,
//!    // So if you want to avoid conflict, you can use a explicit name respect to your crate name.
//!    // It support multiple definition, with Generic enabled.
//!    mod m {
//!        use trait_cast_rs::{UniqueTypeId, UniqueId, unique_id};
//!        pub trait Q {}
//!        pub trait W {}
//!        pub trait E<T> {}
//!        unique_id!{
//!           #[UniqueTypeIdVersion((0,1,0))] // default to (0,0,0)
//!           #[UniqueTypeIdFile("types.toml")] // no default
//!           dyn m::Q;
//!           dyn W;
//!           dyn E<u8>;
//!           A;
//!           B<u8>;
//!        }
//!        pub struct A;
//!        pub struct B<T> {
//!           pub t: T
//!        }
//!        impl Q for A {}
//!    }
//!    use m::*;
//!    assert_ne!(<dyn Q>::TYPE_ID.0, 0u64);
//!}
//! ```

use core::fmt;

use trait_cast_impl_rs::{unique_id, unique_id_without_version_hash};

/// A strong type for type id.
#[repr(transparent)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct UniqueId(pub u64);

impl fmt::Display for UniqueId {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

/// A trait for providing a type id number.
pub trait UniqueTypeId {
  /// The type name defined by the user, more unique and stable name than the [`core::any::type_name`]
  const TYPE_NAME: &'static str;
  /// A unique id for a type, used by trait_cast to cast trait objects.
  /// If not equal, trait_cast will fail.
  const TYPE_ID: UniqueId;
  /// A version for a type, with out pre release, build meta etc.
  /// Used by trait_cast to check version compatibility. If versions are not compatible, trait_cast will fail.
  const TYPE_VERSION: (u64, u64, u64);

  /// Returns the type name.
  fn ty_name(&self) -> &'static str;

  /// Returns the type id number.
  fn ty_id(&self) -> UniqueId;

  /// Returns the version for a type
  fn ty_version(&self) -> (u64, u64, u64);
}

// implement the trait for primitive types in prelude
unique_id_without_version_hash! {
  u8;
  u16;
  u32;
  u64;
  i8;
  i16;
  i32;
  i64;
  f32;
  f64;
  bool;
  char;
  String;
  str;
  &str;
}

// implement the trait for optional types of primitive types in prelude
unique_id_without_version_hash! {
  Option<u8>;
  Option<u16>;
  Option<u32>;
  Option<u64>;
  Option<i8>;
  Option<i16>;
  Option<i32>;
  Option<i64>;
  Option<f32>;
  Option<f64>;
  Option<bool>;
  Option<char>;
  Option<String>;
  Option<&str>;
}

#[cfg(test)]
mod tests {
  use trait_cast_impl_rs::{unique_id, unique_id_without_version_hash};

  use super::*;

  #[test]
  fn test_unique_id_typeid_equal_to() {
    pub struct A1;
    pub struct A2;
    unique_id_without_version_hash! {
      #[UniqueTypeIdVersion((0,1,0))]
      A1;
    }
    unique_id_without_version_hash! {
      #[UniqueTypeIdVersion((0,2,0))]
      #[TypeIdEqualTo("A1")]
      A2;
    }

    assert_eq!(<A1 as UniqueTypeId>::TYPE_NAME, "A1");
    assert_eq!(<A2 as UniqueTypeId>::TYPE_NAME, "A2");
    assert_eq!(<A1 as UniqueTypeId>::TYPE_ID, <A2 as UniqueTypeId>::TYPE_ID);
    assert_eq!(<A1 as UniqueTypeId>::TYPE_VERSION, (0, 1, 0));
    assert_eq!(<A2 as UniqueTypeId>::TYPE_VERSION, (0, 2, 0));
  }

  #[test]
  fn test_unique_id_generic_ne() {
    pub struct A<T> {
      pub _t: T,
    }
    unique_id! {
      A<u8>;
      A<u16>;
    }
    assert_eq!(<A<u8> as UniqueTypeId>::TYPE_NAME, "A<u8>");
    assert_eq!(<A<u16> as UniqueTypeId>::TYPE_NAME, "A<u16>");
    assert_ne!(
      <A<u8> as UniqueTypeId>::TYPE_ID,
      <A<u16> as UniqueTypeId>::TYPE_ID
    );
    assert_eq!(
      <A<u8> as UniqueTypeId>::TYPE_VERSION,
      <A<u16> as UniqueTypeId>::TYPE_VERSION
    );
    assert_eq!(<A<u8> as UniqueTypeId>::TYPE_VERSION, (0, 0, 0));
    assert_eq!(<A<u16> as UniqueTypeId>::TYPE_VERSION, (0, 0, 0));
    let x = "xf";
    fn f<T: UniqueTypeId>(x: T) {
      assert_eq!(x.ty_id(), <T as UniqueTypeId>::TYPE_ID);
    }
    f(x);
  }
}
