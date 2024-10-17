use crate::make_trait_castable_decl;
use crate::{unique_id, UniqueId, UniqueTypeId};

const fn _test_empty_trait_cast_targets() {
  struct Woof {}

  make_trait_castable_decl! {
    Woof => (),
  }
}

mod m {
  use crate::{unique_id, UniqueId, UniqueTypeId};
  pub trait Q {}
  pub trait W {}
  pub trait E<T> {}
  unique_id! {
      #[UniqueTypeIdVersion((0,1,0))] // default to (0,0,0)
      #[UniqueTypeIdFile("types.toml")] // no default
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
