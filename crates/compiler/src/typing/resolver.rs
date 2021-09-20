impl Resolver {
  pub fn open_for_definition(&mut self) -> DefinitionWindow {
      debug_assert!(self.state == State::Closed);
      debug_assert!(self.captureStack.empty());

      self.state = State::Open;
      DefinitionWindow {
          inner: self,
      }
  }

  pub fn resolveAll(&mut self)  {
      debug_assert!(self.state == State::Open);
      debug_assert!(self.captureStack.empty());
  
      self.state = State::Closing;
  
      while !self.resolutionQueue.isEmpty() {
        self.doResolve(self.resolutionQueue.removeFirst());
      }
  
      self.state = State::Closed;
  
      // TODO(sdh): Stop doing this here. It's obviously the wrong place.
      // By default, the global "this" type is just an anonymous object.
      // If the user has defined a Window type, make the Window the
      // implicit prototype of "this".
      // PrototypeObjectType globalThis =
      //     (PrototypeObjectType) this.registry.getNativeType(JSTypeNative.GLOBAL_THIS);
      // JSType windowType = this.registry.getGlobalType("Window");
      // if (globalThis.isUnknownType()) {
      //   ObjectType windowObjType = ObjectType.cast(windowType);
      //   if (windowObjType != null) {
      //     globalThis.setImplicitPrototype(windowObjType);
      //   } else {
      //     globalThis.setImplicitPrototype(
      //         this.registry.getNativeObjectType(JSTypeNative.OBJECT_TYPE));
      //   }
      // }
  }

  fn doResolve(ty: Ty) {
      ty.resolve(this.registry.getErrorReporter());
    }
}

#[derive(PartialEq, Eq)]
enum State {
  Closed,
  Open,
  Closing,
}

pub struct DefinitionWindow<'w> {
  inner: &'w mut Resolver,
}
impl Deref for DefinitionWindow<'_> {
  type Target = Resolver;

  fn deref(&self) -> &Resolver {
      &self.inner
  }
}
impl DerefMut for DefinitionWindow<'_> {
  fn deref_mut(&mut self) -> &mut Resolver {
      &mut self.inner
  }
}

impl Drop for DefinitionWindow<'_> {
  fn drop(&mut self) {
      self.inner.resolveAll();
  }
}