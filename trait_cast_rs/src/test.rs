use crate::make_trait_castable_decl;
use fixed_type_id::{self as __fixed_type_id, FixedId, FixedTypeId, FixedVersion};

const fn _test_empty_trait_cast_targets() {
  struct Woof {}

  make_trait_castable_decl! {
    Woof => (),
  }
}

mod m {
  use fixed_type_id::{self as __fixed_type_id, fixed_type_id, FixedId, FixedTypeId, FixedVersion};
  pub trait Q {}
  pub trait W {}
  pub trait E<T> {}
  fixed_type_id! {
      #[FixedTypeIdVersion((0,1,0))] // default to (0,0,0)
      #[FixedTypeIdFile("types.toml")] // no default
      dyn trait_cast_rs::m::Q;
      dyn W;
      dyn E<u8>;
      A;
      B<u8>;
  }
  pub struct A;
  pub struct B<T> {
    pub t: T,
  }
  impl Q for A {}
}
