{-# LANGUAGE ExistentialQuantification #-}

module Intrusive where

import           Control.Monad (forever)

data ShouldTrace = Trace | NoTrace
type Traceable a = (ShouldTrace, a)
trace   :: a -> Traceable a
noTrace :: a -> Traceable a
trace   = \x -> (Trace      , x)
noTrace = \x -> (NoTrace    , x)

data IOPipe a b = forall c. IOPipe (a -> IO c) (c -> IO b)
a >>> b = IOPipe a b
forever_ :: IOPipe a a -> [IOPipe a a]
forever_ x = let ret = x : ret in ret

runIOPipe :: [IOPipe () ()] -> IO ()
runIOPipe (IOPipe a b : xs) = forever $ return () >>= a >>= b
