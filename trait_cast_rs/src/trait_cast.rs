use core::{
  any::{type_name, Any, TypeId},
  fmt::{self, Debug, Formatter},
  ptr,
  ptr::DynMetadata,
};

#[cfg(feature = "alloc")]
use alloc::{boxed::Box, rc::Rc, sync::Arc};

/// This trait must be implemented on every concrete type for every trait that `TraitcastableAny`
/// should be able to downcast to.
///
/// This trait is not object save.
pub trait TraitcastableTo<Target: 'static + ?Sized>: TraitcastableAny {
  /// The metadata that is required to for the cast
  const METADATA: DynMetadata<Target>;
}

/// A struct representing the transformation from `dyn TraitcastableAny` to another `dyn Trait`.
///
/// This should generally not be manually used, but generated by the `make_trait_castable` attribute macro.
pub struct TraitcastTarget {
  target_type_id: crate::UniqueId,
  target_type_name: &'static str,
  /// Must point to the DynMetadata<T> (where T is the type in TypeId)
  metadata: *const (),
}
impl TraitcastTarget {
  /// Creates a new `TraitcastTarget` from a `TraitcastableTo` implementation.
  #[must_use]
  pub const fn from<
    Src: TraitcastableTo<Target>,
    Target: 'static + ?Sized + crate::UniqueTypeId,
  >() -> Self {
    #[allow(clippy::borrow_as_ptr)] // Seems like another false positive
    Self {
      target_type_id: Target::TYPE_ID,
      target_type_name: type_name::<Target>(),
      metadata: (&Src::METADATA as *const core::ptr::DynMetadata<Target>).cast::<()>(),
    }
  }
  /// Returns the type_id of the type to which can be cast with this instance.
  #[must_use]
  pub const fn target_type_id(&self) -> crate::UniqueId {
    self.target_type_id
  }

  /// Returns the type_name of the type to which can be cast with this instance.
  #[must_use]
  pub const fn target_type_name(&self) -> &'static str {
    self.target_type_name
  }
}

/// A trait marking a type as being potentially able to traitcast from `dyn TraitcastableAny` to another `dyn Trait`.
/// Use this trait instead of the `Any` trait throughout your program.
///
/// This should generally not be manually implemented, but generated by the `make_trait_castable` attribute macro.
///
/// # Safety
/// The function `traitcast_targets' must only produce valid TraitcastTarget (That use the metadata associated the the correct source struct).
/// The function `find_traitcast_target` must not return `Some` unless contained value has the correct target TypeId.
pub unsafe trait TraitcastableAny: Any {
  /// This function returns a list of all the `TraitcastTarget`'s to which a trait object can be cast, this is then used by the implementations of `TraitcastableAnyInfra` to accomplish the traitcast.
  /// The function is used to generate debug output for `TraitcastableAny`.
  /// The default implementation of `find_traitcast_target` uses this function by default.
  ///
  /// This should generally not be manually implemented, but generated by the `make_trait_castable` attribute macro.
  // Note: We dropped `'static` on the return type to better support generics.
  // > "use of generic parameter from outer function"
  fn traitcast_targets(&self) -> &[TraitcastTarget];

  /// This function can be implemented to support custom `TypeId` lookup algorithms.
  /// This may be desired when there are lots of `TraitcastTarget`s (30 or more).
  ///
  /// Possible strategies:
  /// * Unsorted `Vec<TraitcastTarget>` lookup. Hot traits first. - Used by the default implementation.
  /// * `Vec<TraitcastTarget>` sorted by the `TypeId` and performing a binary search on it - Used if feature `const_sort` is used.
  /// * HashMap
  fn find_traitcast_target(&self, target: crate::UniqueId) -> Option<&TraitcastTarget> {
    self.traitcast_targets().iter().find(|possible| {
      possible.target_type_id == target
    })
  }

  /// Returns the `TypeId` of the concrete type.
  fn type_id(&self) -> TypeId {
    Any::type_id(self)
  }
}

/// Mimics the API of `Any` but additionally allows downcasts to select trait objects.
// This helper trait was created to be able to use `min_specialization` to generate different implementations for `Sized` and `!Sized` types.
// The functions actually belong to `TraitcastableAny`.
pub trait TraitcastableAnyInfra<Target: ?Sized>: 'static {
  /// Returns true if `Target` is the exact same type as Self.
  fn is(&self) -> bool;

  /// Returns true if Self can be converted to a `Target`.
  fn can_be(&self) -> bool;

  /// Returns some reference to the inner value if it is downcastable to `Target`, or `None` if it isn’t.
  ///
  /// If `Target` is Sized this is forwarded to `Any::downcast_ref`,
  /// otherwise `TraitcastableAny::traitcast_targets` is used to determine if a traitcast is possible.
  ///
  /// Returns `None` if the concrete type of self is not `Target` and a traitcast is not possible.
  fn downcast_ref(&self) -> Option<&Target>;

  /// Unchecked variant of `downcast_ref`
  #[cfg(feature = "downcast_unchecked")]
  #[doc(cfg(feature = "downcast_unchecked"))]
  unsafe fn downcast_ref_unchecked(&self) -> &Target;

  /// Returns some mutable reference to the inner value if it is downcastable to `Target`, or `None` if it isn’t.
  ///
  /// If `Target` is Sized this is forwarded to `Any::downcast_ref`,
  /// otherwise `TraitcastableAny::traitcast_targets` is used to determine if a traitcast is possible.
  ///
  /// Returns `None` if the concrete type of self is not `Target` and a traitcast is not possible.
  fn downcast_mut(&mut self) -> Option<&mut Target>;

  /// Unchecked variant of `downcast_ref`
  #[cfg(feature = "downcast_unchecked")]
  #[doc(cfg(feature = "downcast_unchecked"))]
  unsafe fn downcast_mut_unchecked(&mut self) -> &mut Target;
}

// TODO: Allocator api support.

/// Extension Trait to implement over Smart Pointer Types (`Box`, `Rc`, `Arc`).
///
/// Tries to mimic the API of `Any` but additionally allows downcasts to select trait objects.
#[cfg(feature = "alloc")]
#[doc(cfg(feature = "alloc"))]
pub trait TraitcastableAnyInfraExt<Target: ?Sized + 'static>: Sized {
  /// The type that will be returned on a successful cast. Something like `Box<Target>`.
  type Output;

  /// Same as `downcast_ref` and `downcast_mut`, except that it downcasts a `Box` in place.
  ///
  /// Returns `None` if the concrete type of self is not `Target` and a traitcast is not possible.
  ///
  /// # Errors
  /// In case a cast is impossible the original input is returned as the error type.
  /// Otherwise the box would be dropped.
  fn downcast(self) -> Result<Self::Output, Self>;

  /// Unchecked variant of `downcast`
  #[cfg(feature = "downcast_unchecked")]
  #[doc(cfg(feature = "downcast_unchecked"))]
  unsafe fn downcast_unchecked(self) -> Self::Output;
}

#[cfg(feature = "min_specialization")]
// Safety:
// Since `traitcast_targets` returns nothing this is always safe.
// `find_traitcast_target` has the default implementations
unsafe impl<T: 'static> TraitcastableAny for T {
  default fn traitcast_targets(&self) -> &[TraitcastTarget] {
    &[]
  }
  default fn find_traitcast_target(&self, target: crate::UniqueTypeId) -> Option<&TraitcastTarget> {
    // Note: copied from above
    self
      .traitcast_targets()
      .iter()
      .find(|possible| possible.target_type_id == target)
  }
}
impl Debug for dyn TraitcastableAny {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "TraitcastableAny to {{")?;
    for (i, target) in self.traitcast_targets().iter().enumerate() {
      if i != 0 {
        write!(f, ", ")?;
      }
      write!(f, "{}", target.target_type_name)?;
    }
    write!(f, "}}")
  }
}
macro_rules! implement_with_markers {
  ($($(+)? $traits:ident)*) => {
    impl<Target: ?Sized + 'static + crate::UniqueTypeId + $($traits +)*> TraitcastableAnyInfra<Target> for dyn TraitcastableAny $(+ $traits)* {
      default fn is(&self) -> bool {
        false
      }
      default fn can_be(&self) -> bool {
        let found_target = self.find_traitcast_target(Target::TYPE_ID);
        found_target.is_some()
      }

      default fn downcast_ref(&self) -> Option<&Target> {
        // SAFETY:
        // The invariant of Traitcast target guarantees that the metadata points to an instance of `<Target as ::core::ptr::Pointee>::Metadata`.
        let metadata = Self::find_traitcast_target(self, Target::TYPE_ID).map(|target| unsafe {*(target.metadata.cast::<<Target as ::core::ptr::Pointee>::Metadata>())});

        let raw_ptr = (self as *const Self).to_raw_parts().0;

        metadata.map(|metadata| {
          let ret_ptr: *const Target = ptr::from_raw_parts(raw_ptr, metadata);

          // SAFETY:
          // we turned this into a raw pointer before and changed the metadata to that of a dyn Trait
          //  where the Trait must be implemented, so this must be safe!
          unsafe {&*ret_ptr}
        })
      }
      #[cfg(feature = "downcast_unchecked")]
      default unsafe fn downcast_ref_unchecked(&self) -> &Target {
        self.downcast_ref().unwrap_unchecked()
      }

      default fn downcast_mut(&mut self) -> Option<&mut Target> {
        // SAFETY:
        // The invariant of Traitcast target guarantees that the metadata points to an instance of `<Target as ::core::ptr::Pointee>::Metadata`.
        let metadata = Self::find_traitcast_target(self, Target::TYPE_ID).map(|target| unsafe {*(target.metadata.cast::<<Target as ::core::ptr::Pointee>::Metadata>())});

        let raw_ptr = (self as *mut Self).to_raw_parts().0;

        metadata.map(|metadata| {
          let ret_ptr: *mut Target = ptr::from_raw_parts_mut(raw_ptr, metadata);

          // SAFETY:
          // we turned this into a raw pointer before and changed the metadata to that of a dyn Trait
          //  where the Trait must be implemented, so this must be safe!
          unsafe {&mut *ret_ptr}
        })

      }
      #[cfg(feature = "downcast_unchecked")]
      default unsafe fn downcast_mut_unchecked(&mut self) -> &mut Target {
        self.downcast_mut().unwrap_unchecked()
      }
    }
    impl<Target: Sized + 'static + crate::UniqueTypeId + $($traits +)*> TraitcastableAnyInfra<Target> for dyn TraitcastableAny $(+ $traits)* {
      fn is(&self) -> bool {
        <dyn Any>::is::<Target>(self)
      }
      fn can_be(&self) -> bool {
        <dyn TraitcastableAny as TraitcastableAnyInfra<Target>>::is(self)
      }
      fn downcast_ref(&self) -> Option<&Target> {
        <dyn Any>::downcast_ref::<Target>(self)
      }
      #[cfg(feature = "downcast_unchecked")]
      unsafe fn downcast_ref_unchecked(&self) -> &Target {
        <dyn Any>::downcast_ref_unchecked::<Target>(self)
      }

      fn downcast_mut(&mut self) -> Option<&mut Target> {
        <dyn Any>::downcast_mut::<Target>(self)
      }
      #[cfg(feature = "downcast_unchecked")]
      unsafe fn downcast_mut_unchecked(&mut self) -> &mut Target {
        <dyn Any>::downcast_mut_unchecked::<Target>(self)
      }
    }
  };
}

#[cfg(feature = "alloc")]
impl<Src: TraitcastableAnyInfra<Target> + ?Sized, Target: ?Sized + 'static>
  TraitcastableAnyInfraExt<Target> for Box<Src>
{
  type Output = Box<Target>;

  default fn downcast(self) -> Result<Self::Output, Self> {
    let raw = Self::into_raw(self);
    // SAFETY:
    // We can cast the *mut to a &mut since we never use the pointer directly in the success case
    //  and the reference isn't passed to the failure case.
    if let Some(to_ref) = unsafe { &mut *raw }.downcast_mut() {
      // SAFETY:
      // The pointer originates from a `Box` with the same dynamic type,
      //  since we only changed the pointer metadata.
      Ok(unsafe { Box::from_raw(to_ref) })
    } else {
      // SAFETY:
      // We reconstruct the previously destructed `Box`.
      Err(unsafe { Self::from_raw(raw) })
    }
  }
  #[cfg(feature = "downcast_unchecked")]
  default unsafe fn downcast_unchecked(self) -> Self::Output {
    <Self as TraitcastableAnyInfraExt<Target>>::downcast(self).unwrap_unchecked()
  }
}

#[cfg(feature = "alloc")]
impl<Src: TraitcastableAnyInfra<Target>, Target: Sized + 'static> TraitcastableAnyInfraExt<Target>
  for Box<Src>
{
  fn downcast(self) -> Result<Self::Output, Self> {
    #[cfg(feature = "downcast_unchecked")]
    if TraitcastableAnyInfra::<Target>::is(self.as_ref()) {
      // SAFETY:
      // We checked for dynamic type equality `is` in the previous if.
      unsafe { Ok(<Box<dyn Any>>::downcast_unchecked(self)) }
    } else {
      Err(self)
    }
    #[cfg(not(feature = "downcast_unchecked"))]
    if TraitcastableAnyInfra::<Target>::is(self.as_ref()) {
      Ok(<Box<dyn Any>>::downcast::<Target>(self).unwrap())
    } else {
      Err(self)
    }
  }

  #[cfg(feature = "downcast_unchecked")]
  unsafe fn downcast_unchecked(self) -> Self::Output {
    <Box<dyn Any>>::downcast_unchecked::<Target>(self)
  }
}

#[cfg(feature = "alloc")]
impl<Src: TraitcastableAnyInfra<Target> + ?Sized, Target: ?Sized + 'static>
  TraitcastableAnyInfraExt<Target> for Rc<Src>
{
  type Output = Rc<Target>;

  default fn downcast(self) -> Result<Self::Output, Self> {
    let raw = Self::into_raw(self);
    // SAFETY:
    // We can cast the *mut to a &mut since we never use the pointer directly in the success case
    //  and the reference isn't passed to the failure case.
    if let Some(to_ref) = unsafe { &*raw }.downcast_ref() {
      // SAFETY:
      // The pointer originates from a `Rc` with the same dynamic type,
      //  since we only changed the pointer metadata.
      Ok(unsafe { Rc::from_raw(to_ref) })
    } else {
      // SAFETY:
      // We reconstruct the previously destructed `Rc`.
      Err(unsafe { Self::from_raw(raw) })
    }
  }
  #[cfg(feature = "downcast_unchecked")]
  default unsafe fn downcast_unchecked(self) -> Self::Output {
    <Self as TraitcastableAnyInfraExt<Target>>::downcast(self).unwrap_unchecked()
  }
}

#[cfg(feature = "alloc")]
impl<Src: TraitcastableAnyInfra<Target>, Target: Sized + 'static> TraitcastableAnyInfraExt<Target>
  for Rc<Src>
{
  fn downcast(self) -> Result<Self::Output, Self> {
    #[cfg(feature = "downcast_unchecked")]
    if TraitcastableAnyInfra::<Target>::is(self.as_ref()) {
      // SAFETY:
      // We checked for dynamic type equality `is` in the previous if.
      unsafe { Ok(<Rc<dyn Any>>::downcast_unchecked(self)) }
    } else {
      Err(self)
    }
    #[cfg(not(feature = "downcast_unchecked"))]
    if TraitcastableAnyInfra::<Target>::is(self.as_ref()) {
      Ok(<Rc<dyn Any>>::downcast(self).unwrap())
    } else {
      Err(self)
    }
  }

  #[cfg(feature = "downcast_unchecked")]
  unsafe fn downcast_unchecked(self) -> Self::Output {
    <Rc<dyn Any>>::downcast_unchecked::<Target>(self)
  }
}

#[cfg(feature = "alloc")]
impl<
    Src: TraitcastableAnyInfra<Target> + ?Sized + Send + Sync,
    Target: ?Sized + 'static + Send + Sync,
  > TraitcastableAnyInfraExt<Target> for Arc<Src>
{
  type Output = Arc<Target>;

  default fn downcast(self) -> Result<Self::Output, Self> {
    let raw = Self::into_raw(self);
    // SAFETY:
    // We can cast the *mut to a &mut since we never use the pointer directly in the success case
    //  and the reference isn't passed to the failure case.
    if let Some(to_ref) = unsafe { &*raw }.downcast_ref() {
      // SAFETY:
      // The pointer originates from a `Arc` with the same dynamic type,
      //  since we only changed the pointer metadata.
      Ok(unsafe { Arc::from_raw(to_ref) })
    } else {
      // SAFETY:
      // We reconstruct the previously destructed `Arc`.
      Err(unsafe { Self::from_raw(raw) })
    }
  }
  #[cfg(feature = "downcast_unchecked")]
  default unsafe fn downcast_unchecked(self) -> Self::Output {
    <Self as TraitcastableAnyInfraExt<Target>>::downcast(self).unwrap_unchecked()
  }
}

#[cfg(feature = "alloc")]
impl<Src: TraitcastableAnyInfra<Target> + Send + Sync, Target: Sized + 'static + Send + Sync>
  TraitcastableAnyInfraExt<Target> for Arc<Src>
{
  fn downcast(self) -> Result<Self::Output, Self> {
    #[cfg(feature = "downcast_unchecked")]
    if TraitcastableAnyInfra::<Target>::is(self.as_ref()) {
      // SAFETY:
      // We checked for dynamic type equality `is` in the previous if.
      unsafe { Ok(<Arc<dyn Any + Send + Sync>>::downcast_unchecked(self)) }
    } else {
      Err(self)
    }
    #[cfg(not(feature = "downcast_unchecked"))]
    if TraitcastableAnyInfra::<Target>::is(self.as_ref()) {
      Ok(<Arc<dyn Any + Send + Sync>>::downcast(self).unwrap())
    } else {
      Err(self)
    }
  }

  #[cfg(feature = "downcast_unchecked")]
  unsafe fn downcast_unchecked(self) -> Self::Output {
    <Arc<dyn Any + Send + Sync>>::downcast_unchecked::<Target>(self)
  }
}

implement_with_markers!();
implement_with_markers!(Send);
implement_with_markers!(Send + Sync);
