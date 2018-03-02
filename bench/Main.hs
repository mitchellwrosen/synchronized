{-# language RankNTypes      #-}
{-# language TemplateHaskell #-}

import Control.Concurrent.Synchronized

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad
import Data.IORef
import Gauge
import Gauge.Main

main :: IO ()
main = do
  defaultMain
    [ bench "synchronized 1 1x1000"   $ run ($synchronized 1) 1   1000
    , bench "synchronized 1 10x1000"  $ run ($synchronized 1) 10  1000
    , bench "synchronized 1 100x1000" $ run ($synchronized 1) 100 1000

    , bench "synchronized 2 1x1000"   $ run ($synchronized 2) 1   1000
    , bench "synchronized 2 10x1000"  $ run ($synchronized 2) 10  1000
    , bench "synchronized 2 100x1000" $ run ($synchronized 2) 100 1000
    ]

run :: (forall a. IO a -> IO a) -> Int -> Int -> Benchmarkable
run sync n m = whnfIO $ do
  ref <- newIORef (0 :: Int)
  doneVar <- newEmptyMVar
  replicateM_ n $
    forkIO $ do
      replicateM_ m (sync (modifyIORef' ref (+1)))
      putMVar doneVar ()
  replicateM_ n (takeMVar doneVar)

instance NFData (TVar a) where rnf x = x `seq` ()
