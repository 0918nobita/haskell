import Control.Monad.State (StateT(StateT), runStateT, lift)

-- newtype StateT s (m :: * -> *) a = StateT { runStateT :: s -> m (a, s) }

stA :: StateT s IO ()
stA = StateT innerFunc
  where
    innerFunc :: s -> IO ((), s)
    innerFunc s = print "A" >>= (\a -> return (a, s))

-- lift :: (MonadTrans t, Monad m) => m a -> t m a

stB :: StateT s IO ()
stB = lift $ print "B"

-- runStateT :: StateT s m a -> s -> m (a, s)

main :: IO ()
main =
  runStateT stA 10
  >>= print  -- "A", ((), 10)
  >> runStateT stB 20
  >>= print  -- "B", ((), 20)
