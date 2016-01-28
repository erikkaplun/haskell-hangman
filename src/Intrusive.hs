{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Intrusive where

import           Control.Monad (forever, void)

data ShouldTrace = Trace | NoTrace
type Traceable a = (ShouldTrace, a)
trace   :: a -> Traceable a
noTrace :: a -> Traceable a
trace   = \x -> (Trace      , x)
noTrace = \x -> (NoTrace    , x)


data IOArrow :: * -> * -> * where
  IOPipe  :: forall a b c. (a -> IO c) -> (c -> IO b) -> IOArrow a b
  IOForev :: forall a    . IOArrow a a                -> IOArrow a a

(>>>)   = IOPipe
ioForev = IOForev

runIOPipe  :: IOArrow a  b  ->  a -> IO       b
runIOPipe' :: IOArrow () b        -> IO       b

runIOPipe (IOPipe  a b ) = \x -> a x  >>= b
runIOPipe (IOForev pipe) = \x -> forever $ runIOPipe pipe x

runIOPipe' x = runIOPipe x ()

runIOPipe_  ::  IOArrow  a  b  ->  a -> IO                   ()
runIOPipe'_ ::  IOArrow  () b        -> IO                   ()
runIOPipe_  pipe x = void $ runIOPipe pipe x
runIOPipe'_ pipe   = void $ runIOPipe pipe ()
