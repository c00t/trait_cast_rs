// #![cfg_attr(feature = "min_specialization", feature(min_specialization))]
// #![cfg_attr(feature = "downcast_unchecked", feature(downcast_unchecked))]
#![cfg_attr(feature = "const_sort", feature(const_trait_impl, const_mut_refs))]
#![feature(ptr_metadata)]
#![feature(downcast_unchecked)]
#![feature(min_specialization)]

use trait_cast_rs::{
  make_trait_castable, make_trait_castable_decl, make_trait_castable_decl_with_version,
  TraitcastableAny, TraitcastableAnyInfra, TraitcastableAnyInfraExt,
};

#[make_trait_castable(Dog, Cat)]
enum HybridPet {
  Name(String),
}
impl HybridPet {
  fn greet(&self) {
    let Self::Name(name) = self;
    println!("{name}: Hi")
  }
}

use fixed_type_id::prelude::*;

fixed_type_id! {
  dyn Dog
}

fixed_type_id! {
  dyn Cat
}

impl Dog for HybridPet {
  fn bark(&self) {
    let Self::Name(name) = self;
    println!("{name}: Woof!")
  }
}

impl Cat for HybridPet {
  fn meow(&self) {
    let Self::Name(name) = self;
    println!("{name}: Meow!")
  }
}

trait Dog {
  fn bark(&self);
}
trait Cat {
  fn meow(&self);
}
#[cfg_attr(test, test)]
fn main() {
  // The box is technically not needed but kept for added realism
  let pet = Box::new(HybridPet::Name("Kokusnuss".to_string()));
  pet.greet();

  let castable_pet: Box<dyn TraitcastableAny> = pet;

  let as_dog: &dyn Dog = castable_pet.downcast_ref().unwrap();
  as_dog.bark();

  let as_cat: &dyn Cat = castable_pet.downcast_ref().unwrap();
  as_cat.meow();

  let cast_back: &HybridPet = castable_pet.downcast_ref().unwrap();
  cast_back.greet();

  let box_pet: Box<HybridPet> = castable_pet.downcast().unwrap();
  box_pet.greet();
}
