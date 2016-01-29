{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Intrusive where

import           Control.Arrow
import           Control.Category
import           Control.Monad (forever, void)
import           Prelude       hiding ((.))

data ShouldTrace = Trace | NoTrace
type Traceable a = (ShouldTrace, a)
trace   :: a -> Traceable a
noTrace :: a -> Traceable a
trace   = \x -> (Trace      , x)
noTrace = \x -> (NoTrace    , x)

data IOArrow :: * -> * -> * where
  IOId    :: IOArrow a a
  IOConst :: b -> IOArrow a b
  IOStage :: (a -> IO b) -> IOArrow a b
  IOPipe  :: forall a b c. (a -> IO c) -> (c -> IO b) -> IOArrow a b
  IOForev :: forall a    . IOArrow a a                -> IOArrow a a

(>>>) = IOPipe
ioForev = IOForev

instance Functor (IOArrow a) where
  fmap f IOId         = IOStage (return . f)
  fmap f (IOStage g)  = IOStage (fmap f . g)
  fmap f (IOPipe g h) = undefined
  fmap f (IOForev _)  = undefined

instance Applicative (IOArrow a) where
  pure x    = IOConst x
  af <*> ax = IOStage $ \a -> do f <- runIOArrow af a
                                 x <- runIOArrow ax a
                                 return (f x)

instance Category IOArrow where
  id = IOId
  f . g = IOStage $ \b -> runIOArrow g b >>= runIOArrow f

instance Arrow IOArrow where
  arr f = IOStage (return . f)
  first a = IOStage $ \(b, d) -> runIOArrow a b >>= return . (,d)

runIOArrow  :: IOArrow a  b  ->  a -> IO       b
runIOArrow IOId           = return
runIOArrow (IOStage  f)   = \x   -> f x
runIOArrow (IOPipe  a b ) = \x -> a x  >>= b
runIOArrow (IOForev pipe) = \x -> forever $ runIOArrow pipe x

runIOArrow' :: IOArrow () b        -> IO       b
runIOArrow' x = runIOArrow x ()

runIOArrow_  ::  IOArrow  a  b  ->  a -> IO                   ()
runIOArrow_' ::  IOArrow  () b        -> IO                   ()
runIOArrow_  pipe x = void $ runIOArrow pipe x
runIOArrow_' pipe   = void $ runIOArrow pipe ()

runIOPipe_' = runIOArrow_'  -- don't break Lib.hs
