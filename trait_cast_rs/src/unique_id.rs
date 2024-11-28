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

impl UniqueId {
  /// Get UniqueId of a type
  pub const fn from<Target: 'static + ?Sized + UniqueTypeId>() -> Self {
    Target::TYPE_ID
  }

  /// Get the inner u64 value.
  pub const fn as_u64(&self) -> u64 {
    self.0
  }

  /// Get UniqueId from a type name.
  ///
  /// It can be used inside const context.
  pub const fn from_type_name(type_name: &'static str) -> Self {
    let hash = rapidhash::rapidhash(type_name.as_bytes());
    UniqueId(hash)
  }
}

impl fmt::Display for UniqueId {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

/// A trait for providing a type id number.
pub trait UniqueTypeId {
  /// The type name defined by the user, more unique and stable name than the [`core::any::type_name`]
  ///
  /// When enabled feature `erase_name`, the type name will be a hex string of the hash of the original type name if it's a primitive type without generic.
  /// Otherwise, it will be the original type name.
  ///
  /// You should implement this trait as specific as possible. Because that the generic implement will make your binary size larger.
  const TYPE_NAME: &'static str;
  /// A unique id for a type, used by trait_cast to cast trait objects.
  /// If not equal, trait_cast will fail.
  const TYPE_ID: UniqueId = UniqueId::from_type_name(Self::TYPE_NAME);
  /// A version for a type, with out pre release, build meta etc.
  /// Used by trait_cast to check version compatibility. If versions are not compatible, trait_cast will fail.
  const TYPE_VERSION: (u64, u64, u64) = (0, 0, 0);

  /// Returns the type name.
  fn ty_name(&self) -> &'static str {
    Self::TYPE_NAME
  }

  /// Returns the type id number.
  fn ty_id(&self) -> UniqueId {
    Self::TYPE_ID
  }

  /// Returns the version for a type
  fn ty_version(&self) -> (u64, u64, u64) {
    Self::TYPE_VERSION
  }
}

/// A trait for providing a fixed string for the type name, used to avoid heap when need to format the type name.
///
/// Useful for generic types, which is not possible to be implemented in const.
pub trait FixedTypeName {
  /// A raw slice for the type name, used to create a fixed `fstr`.
  ///
  /// Use a const slice to make it easi
  const RAW_SLICE: &[&str];
  /// A fixed string for the type name, used to avoid heap when need to format the type name.
  const TYPE_NAME_FSTR: fixedstr::fstr<128> = slice_to_fstr(Self::RAW_SLICE);
}

/// A helper function to get the type name of a type.
pub fn type_name<T: ?Sized + UniqueTypeId>() -> &'static str {
  T::TYPE_NAME
}

/// A helper function to get the type id of a type.
pub fn type_id<T: ?Sized + UniqueTypeId>() -> UniqueId {
  T::TYPE_ID
}

/// A helper function to get the version of a type.
pub fn type_version<T: ?Sized + UniqueTypeId>() -> (u64, u64, u64) {
  T::TYPE_VERSION
}

// implement the trait for primitive types in prelude
unique_id_without_version_hash! {
  u8;
  u16;
  u32;
  u64;
  u128;
  usize;
  i8;
  i16;
  i32;
  i64;
  i128;
  isize;
  f32;
  f64;
  bool;
  char;
  String;
  str;
  &str;
}

// Unit type
impl UniqueTypeId for () {
  const TYPE_NAME: &'static str = "()";
}

/// Implements UniqueTypeId and FixedTypeName for wrapper types that delegate to their inner type(s).
///
#[macro_export]
macro_rules! implement_wrapper_unique_id {
  (@impl_generics $wrapper:ident, ($first:ident $(, $rest:ident)*), $prefix:expr) => {
        impl<$first $(, $rest)*> UniqueTypeId for $wrapper<$first $(, $rest)*>
        where
            $first: UniqueTypeId,
            $($rest: UniqueTypeId,)*
            Self: FixedTypeName,
        {
            const TYPE_NAME: &'static str = fstr_to_str(&Self::TYPE_NAME_FSTR);
        }

        impl<$first: UniqueTypeId $(, $rest: UniqueTypeId)*> FixedTypeName for $wrapper<$first $(, $rest)*>
        where
            $first: UniqueTypeId,
            $($rest: UniqueTypeId,)*
        {
            const RAW_SLICE: &[&str] = &[
                $prefix,
                "<",
                $first::TYPE_NAME,
                $(
                    ",",
                    $rest::TYPE_NAME,
                )*
                ">"
            ];
        }
    };

    ($($wrapper:ident<$first:ident $(, $rest:ident)*> => $prefix:expr);* $(;)?) => {
      $(
          implement_wrapper_unique_id!(@impl_generics $wrapper, ($first $(, $rest)*), $prefix);
      )*
  };
}

const fn fstr_to_str<const N: usize>(fstr: &'static fixedstr::fstr<N>) -> &'static str {
  unsafe { core::str::from_raw_parts(fstr.to_ptr(), fstr.len()) }
}

const fn slice_to_fstr<const N: usize>(slice: &[&str]) -> fixedstr::fstr<N> {
  fixedstr::fstr::<N>::const_create_from_str_slices(slice)
}

use std::collections::{BTreeMap, HashMap};
use std::marker::PhantomData;
implement_wrapper_unique_id! {
  PhantomData<T> => "std::marker::PhantomData";
  Vec<T> => "std::vec::Vec";
  HashMap<K,V> => "std::collections::HashMap";
  Box<T> => "std::boxed::Box";
  BTreeMap<K,V> => "std::collections::BTreeMap";
}

/// Internal macro to implement UniqueTypeId for tuples.
macro_rules! implement_tuple_unique_id {
    () => {
        implement_tuple_unique_id!(@internal 2, T1, T2);
        implement_tuple_unique_id!(@internal 3, T1, T2, T3);
        implement_tuple_unique_id!(@internal 4, T1, T2, T3, T4);
        implement_tuple_unique_id!(@internal 5, T1, T2, T3, T4, T5);
        implement_tuple_unique_id!(@internal 6, T1, T2, T3, T4, T5, T6);
        implement_tuple_unique_id!(@internal 7, T1, T2, T3, T4, T5, T6, T7);
        implement_tuple_unique_id!(@internal 8, T1, T2, T3, T4, T5, T6, T7, T8);
        implement_tuple_unique_id!(@internal 9, T1, T2, T3, T4, T5, T6, T7, T8, T9);
        implement_tuple_unique_id!(@internal 10, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);
        implement_tuple_unique_id!(@internal 11, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
        implement_tuple_unique_id!(@internal 12, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);
    };

    (@internal $n:tt, $first:ident $(, $rest:ident)*) => {
        impl<$first $(, $rest)*> UniqueTypeId for ($first $(, $rest)*)
        where
            $first: UniqueTypeId,
            $($rest: UniqueTypeId,)*
            Self: FixedTypeName,
        {
            const TYPE_NAME: &'static str = fstr_to_str(&<Self as FixedTypeName>::TYPE_NAME_FSTR);
        }

        impl<$first: UniqueTypeId $(, $rest: UniqueTypeId)*> FixedTypeName for ($first $(, $rest)*) {
            const RAW_SLICE: &[&str] = &[
                "(",
                $first::TYPE_NAME,
                $(
                    ",",
                    $rest::TYPE_NAME,
                )*
                ")"
            ];
        }
    };
}

implement_tuple_unique_id!();

/// Internal macro to implement UniqueTypeId for fixed size arrays.
macro_rules! implement_array_unique_id {
  () => {
      implement_array_unique_id!(@internal 0);
      implement_array_unique_id!(@internal 1);
      implement_array_unique_id!(@internal 2);
      implement_array_unique_id!(@internal 3);
      implement_array_unique_id!(@internal 4);
      implement_array_unique_id!(@internal 5);
      implement_array_unique_id!(@internal 6);
      implement_array_unique_id!(@internal 7);
      implement_array_unique_id!(@internal 8);
      implement_array_unique_id!(@internal 9);
      implement_array_unique_id!(@internal 10);
      implement_array_unique_id!(@internal 11);
      implement_array_unique_id!(@internal 12);
      implement_array_unique_id!(@internal 13);
      implement_array_unique_id!(@internal 14);
      implement_array_unique_id!(@internal 15);
      implement_array_unique_id!(@internal 16);
      implement_array_unique_id!(@internal 17);
      implement_array_unique_id!(@internal 18);
      implement_array_unique_id!(@internal 19);
      implement_array_unique_id!(@internal 20);
      implement_array_unique_id!(@internal 21);
      implement_array_unique_id!(@internal 22);
      implement_array_unique_id!(@internal 23);
      implement_array_unique_id!(@internal 24);
      implement_array_unique_id!(@internal 25);
      implement_array_unique_id!(@internal 26);
      implement_array_unique_id!(@internal 27);
      implement_array_unique_id!(@internal 28);
      implement_array_unique_id!(@internal 29);
      implement_array_unique_id!(@internal 30);
      implement_array_unique_id!(@internal 31);
      implement_array_unique_id!(@internal 32);
  };

  (@internal $n:tt) => {
      impl<T: UniqueTypeId> UniqueTypeId for [T; $n]
      where
          Self: FixedTypeName,
      {
          const TYPE_NAME: &'static str = fstr_to_str(&Self::TYPE_NAME_FSTR);
      }

      impl<T: UniqueTypeId> FixedTypeName for [T; $n] {
          const RAW_SLICE: &[&str] = &[
              "[",
              T::TYPE_NAME,
              ";",
              stringify!($n),
              "]"
          ];
      }
  };
}

implement_array_unique_id!();

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
  use std::marker::PhantomData;

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

  #[test]
  fn test_tuple_type() {
    assert_eq!(<(String, u32) as UniqueTypeId>::TYPE_NAME, "(String,u32)");
  }

  #[test]
  fn test_more_types() {
    assert_eq!(
      <Vec<(String, u32)> as UniqueTypeId>::TYPE_NAME,
      "std::vec::Vec<(String,u32)>"
    );

    assert_eq!(
      <PhantomData<i32> as UniqueTypeId>::TYPE_NAME,
      "std::marker::PhantomData<i32>"
    );

    assert_eq!(<((), String) as UniqueTypeId>::TYPE_NAME, "((),String)");

    assert_eq!(<[u8; 10] as UniqueTypeId>::TYPE_NAME, "[u8;10]");
    assert_eq!(
      <[(u8, u32); 20] as UniqueTypeId>::TYPE_NAME,
      "[(u8,u32);20]"
    );
    assert_ne!(
      <[u8; 10] as UniqueTypeId>::TYPE_ID,
      <[(u8, u32); 20] as UniqueTypeId>::TYPE_ID
    );
  }
}
