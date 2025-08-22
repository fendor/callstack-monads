module Stack where

import GHC.Internal.Stack.Decode
import GHC.Stack.Annotation.Experimental
import GHC.Internal.ClosureTypes
import GHC.Internal.Heap.Closures
import GHC.Internal.InfoProv.Types
import Unsafe.Coerce
import GHC.Stack.CloneStack as CloneStack

import Data.Maybe

getStackAnnotations :: IO [SomeStackAnnotation]
getStackAnnotations = do
  snapshot <- CloneStack.cloneMyStack
  res <- decodeStackWithIpe snapshot
  pure $ mapMaybe (uncurry getStackAnnotation) res

getStackAnnotation :: StackFrame -> Maybe InfoProv -> Maybe SomeStackAnnotation
getStackAnnotation frame _ = case frame of
    AnnFrame {annotation = Box someStackAnno } ->
      case unsafeCoerce someStackAnno of
        SomeStackAnnotation ann ->
          Just $ SomeStackAnnotation ann
    _ -> Nothing

