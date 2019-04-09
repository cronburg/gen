{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, InstanceSigs #-}
module Lib
  where

import Control.Monad.State.Lazy

type Internal = Int

newtype Gen st a = Gen { unGen :: StateT (Internal, st) IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (MonadState (Internal, st) (Gen st)) where

  put :: (Internal, st) -> Gen st ()
  put st = modify (const st)

  get :: Gen st (Internal, st)
  get = gets id

runGen :: st -> Gen st a -> IO a
runGen init_st genM = fst <$> runStateT (unGen genM) (0, init_st)

runMe = runGen 'a' (get >>= \x -> return 'c')

