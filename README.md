# CallStack-Monads

Playground for implementing various monads using the Haskell callstack.

## Reader

The `Reader` monad implemented in terms of [`Stack Annotation Frames`]().

The idea is simple, push the read-only value onto the callstack and when `ask`ing for it, decode the Haskell callstack and retrieve the first `StackAnnotation` with the appropriate type.
The classic `local` can also be similarly implemented, first we `ask` for the value, and then we push a new `Stack Annotation Frame`.
Once the computation finishes, the stack frame is popped off the stack, and the old value can be retrieved via `ask`.
We support nested readers with separate types.

The `Reader` monad is a simple wrapper around the underlying monad:

```haskell
newtype Reader e m a = Reader
  { runReader' :: m a
  } deriving (Functor)
```

Strictly speaking, we misuse the `Stack Annotation Frame` to implement a global read-only variable.
