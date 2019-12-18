{-# LANGUAGE DeriveFunctor #-}

data AST r = Proc r | End
  deriving (Show, Functor)
