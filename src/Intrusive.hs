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

data IOPipe a b = forall c. IOPipe (a -> IO c) (c -> IO b)

(>>>) :: forall a b c. (a -> IO c) -> (c -> IO b) -> IOPipe a b
a >>> b = IOPipe a b
forever_ :: IOPipe a a -> [IOPipe a a]
forever_ x = let ret = x : ret in ret

runIOPipe :: [IOPipe () ()] -> IO ()
runIOPipe (IOPipe a b : _) = Control.Monad.forever $ return () >>= a >>= b
runIOPipe _ = undefined


data IOPipe' :: * -> * -> * where
  IOPipe'   :: forall a b c. (a -> IO c) -> (c -> IO b) -> IOPipe' a b
  IOForev :: forall a    . IOPipe' a a                -> IOPipe' a a

(>>>>) :: forall a b c. (a -> IO c) -> (c -> IO b) -> IOPipe' a b
(>>>>) = IOPipe'
ioForever :: forall a. IOPipe' a a -> IOPipe' a a
ioForever = IOForev

runIOPipe'   ::   IOPipe' a  b  ->  a -> IO       b
runIOPipe''  ::   IOPipe' () b        -> IO       b

runIOPipe'    = \case
                 (IOPipe' a  b) -> \x -> a x  >>= b
                 (IOForev pipe) -> forever $ runIOPipe' pipe
runIOPipe'' x = runIOPipe' x ()

runIOPipe'_  ::  IOPipe'  a  b  ->  a -> IO                   ()
runIOPipe''_ ::  IOPipe'  () b        -> IO                   ()
runIOPipe'_  pipe x = void $ runIOPipe' pipe x
runIOPipe''_ pipe   = void $ runIOPipe' pipe ()
