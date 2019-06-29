module Lib where

import           Data.IORef

notReallyAtomicModifyIORef ::  IORef a -> (a -> (a, b)) -> IO b
notReallyAtomicModifyIORef ref f = do
  r <- readIORef ref
  let (a,b) = f r
  writeIORef ref a
  pure b
