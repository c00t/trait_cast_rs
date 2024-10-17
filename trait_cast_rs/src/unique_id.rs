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

/// A strong type for type id.
#[repr(transparent)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct UniqueId(pub u64);

/// A trait for providing a type id number.
pub trait UniqueTypeId {
  /// A unique id for a type
  const TYPE_ID: UniqueId;

  /// Returns the type id number.
  fn id() -> UniqueId;
}
