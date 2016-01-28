module Todo where

data Diff a = Diff a

data TaskSeq a = Begin { diffTS     :: Diff a
                       , continueTS :: TaskSeq a }
               | DoneForNow

infixr 5 `andThen`
andThen :: Diff a -> TaskSeq a -> TaskSeq a
diff `andThen` continue =
  Begin { diffTS = diff
        , continueTS = continue }

project :: TaskSeq String
project =
  Diff "be able to assemble & disassemble & reassemble stages aka arrows"

  `andThen`

  Diff "track all signals"

  `andThen`

  Diff "make ugliness mechanically traceable"

  `andThen`

  Diff  ( "make dependencies statically defined so that working on "
          ++ "a task that depends on a more closely upcoming task is "
          ++ "forbidden by the compiler" )

  `andThen`

  DoneForNow
