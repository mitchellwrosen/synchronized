{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language TemplateHaskell #-}

module Control.Concurrent.Synchronized
  ( synchronized
  ) where

import Control.Concurrent.Synchronized.Internal

import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (bracket_)
import Data.HashMap.Strict (HashMap)
import GHC.Prim (unsafeCoerce#)
import GHC.Types (Any)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.HashMap.Strict as HashMap

-- | @synchronized n action@ executes @action@, while allowing at most @n@
-- threads to execute it concurrently.
--
-- /Note/: The implementation requires @synchronized@ to inline, as it uses the
-- source location as a /monitor/.
synchronized :: Int -> IO a -> IO a
synchronized = \n act ->
  if n <= 1
    then do
      lock <-
        lookupAny loc >>= \case
          Nothing -> do
            lock <- newMVar ()
            newAny loc lock
          Just lock ->
            pure lock
      withMVar lock (const act)
    else do
      sem <-
        lookupAny loc >>= \case
          Nothing -> do
            sem <- newQSem n
            newAny loc sem
          Just sem ->
            pure sem
      bracket_ (waitQSem sem) (signalQSem sem) act
 where
  loc :: Loc
  loc =
    $(location)
{-# INLINE synchronized #-}

locksVar :: TVar (HashMap Loc Any)
locksVar =
  unsafePerformIO (newTVarIO mempty)
{-# NOINLINE locksVar #-}

newAny :: Loc -> a -> IO a
newAny loc val =
  atomically $ do
    locks <- readTVar locksVar
    case HashMap.lookup loc locks of
      Nothing -> do
        writeTVar locksVar (HashMap.insert loc (unsafeCoerce# val) locks)
        pure (unsafeCoerce# val)
      Just lock ->
        pure (unsafeCoerce# lock)

lookupAny :: Loc -> IO (Maybe a)
lookupAny loc =
  atomically (unsafeCoerce# . HashMap.lookup loc <$> readTVar locksVar)
