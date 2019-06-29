{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Concurrent.Async
import           Control.Monad
import           Data.IORef
import           Test.Hspec
import           Test.QuickCheck

import           Lib

main :: IO ()
main = hspec spec

newtype Iterations = Iterations Word
  deriving Show

instance Arbitrary Iterations where
  arbitrary = Iterations . (+100) . abs <$> arbitrary
  shrink (Iterations i) = Iterations <$> shrink i

newtype ConcurrencyLevel = ConcurrencyLevel Word
  deriving Show

instance Arbitrary ConcurrencyLevel where
  arbitrary = ConcurrencyLevel . abs <$> arbitrary
  shrink (ConcurrencyLevel i) = ConcurrencyLevel <$> shrink i

spec = describe "simple tests" $ do
  it "gets the same result in a single thread" $ do
    property $ \(Iterations n) -> do
      -- we'll use this increment function partially applied.
      -- we want to be able to supply an increment so that in later
      -- tests, different threads can have observably different effects.
      let f incr x = (x+incr, x)
      ref1 <- newIORef 1
      ref2 <- newIORef 1
      actualAtomic <- mapM (atomicModifyIORef ref1 . f) [1..n]
      notReallyAtomic <- mapM (notReallyAtomicModifyIORef ref2 . f) [1..n]
      notReallyAtomic `shouldBe` actualAtomic

  it "fails when we pound it hard" $ do
    let f incr x = (x+incr, x)
        iterations = 50
        threads = 20
    ref1 <- newIORef 1
    ref2 <- newIORef 1
    let runConcurrently = withThreadsAndIterations threads iterations

    actualAtomic    <- runConcurrently $ atomicModifyIORef ref1 . f
    notReallyAtomic <- runConcurrently $ notReallyAtomicModifyIORef ref2 . f
    notReallyAtomic `shouldBe` actualAtomic

  it "fails when we pound it hard, but with a property" $ do
    property $ \(ConcurrencyLevel threads, Iterations iterations) -> do
      let f incr x = (x+incr, x)
      let runConcurrently = withThreadsAndIterations threads iterations

      ref1 <- newIORef 1
      ref2 <- newIORef 1

      actualAtomic    <- runConcurrently $ atomicModifyIORef ref1 . f
      notReallyAtomic <- runConcurrently $ notReallyAtomicModifyIORef ref2 . f
      notReallyAtomic `shouldBe` actualAtomic


  it "property fails all of 10 attempts" $ do
    property $ \(ConcurrencyLevel threads, Iterations iterations) -> do
      let f incr x = (x+incr, x)
      let trials = 10
      let runConcurrently = withThreadsAndIterations threads iterations

      ref1 <- newIORef 1
      ref2 <- newIORef 1

      actualAtomic    <- forM [1..trials] $ \_ ->
        runConcurrently $ atomicModifyIORef ref1 . f
      notReallyAtomic <- forM [1..trials] $ \_ ->
        runConcurrently $ notReallyAtomicModifyIORef ref2 . f

      -- we fail only if _every_ run fails.
      (head actualAtomic, or (zipWith (==) actualAtomic notReallyAtomic))
        `shouldBe` (head notReallyAtomic, True)


withThreadsAndIterations threads iterations f = forConcurrently [1..threads] $ replicateM (fromIntegral iterations) . f
