use crate::make_trait_castable_decl;
use fixed_type_id::prelude::*;
use fixed_type_id::FixedTypeId;

const fn _test_empty_trait_cast_targets() {
  struct Woof {}

  make_trait_castable_decl! {
    Woof => (),
  }
}

mod m {
  use fixed_type_id::prelude::*;
  pub trait Q {}
  pub trait W {}
  pub trait E<T> {}
  fixed_type_id! {
      #[version((0,1,0))] // default to (0,0,0)
      #[store_in_file("types.toml")] // no default
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
