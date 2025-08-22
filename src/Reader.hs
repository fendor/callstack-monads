{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
module Reader where

import GHC.Stack.Annotation.Experimental
import Data.Typeable
import Data.Maybe
import Stack

newtype Reader e m a = Reader
  { runReader' :: m a
  } deriving (Functor)

instance Applicative m => Applicative (Reader e m) where
  pure a = Reader (pure a)
  Reader f <*> Reader a = Reader $ f <*> a

instance Monad m => Monad (Reader e m) where
  x >>= f = Reader $ do
    v <- runReader' x
    runReader' $ f v

data ReaderValue a where
  ReaderValue :: a -> ReaderValue a

instance Typeable a => StackAnnotation (ReaderValue a) where
  displayStackAnnotation (ReaderValue a) = "ReaderValue of type: " <> show (typeOf a)

deriving instance Show a => Show (ReaderValue a)

runReader :: Typeable e => e -> Reader e IO a -> IO a
runReader val act = annotateStackIO (ReaderValue val) $ do
  runReader' act

ask :: forall e . Typeable e => Reader e IO e
ask = do
  annos <- Reader getStackAnnotations

  case mapMaybe tryCast annos of
    [] -> error "No reader value found on stack!"
    (ReaderValue x:xs) -> pure x
  where
    tryCast :: SomeStackAnnotation -> Maybe (ReaderValue e)
    tryCast (SomeStackAnnotation ann) = cast ann

local :: Typeable e => (e -> e) -> Reader e IO a -> Reader e IO a
local f act = do
  e <- ask
  a <- Reader $ annotateStackIO (ReaderValue $ f e) $ do
    runReader' act
  pure a
