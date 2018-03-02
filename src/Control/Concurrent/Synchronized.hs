{-# language DeriveGeneric   #-}
{-# language LambdaCase      #-}
{-# language MagicHash       #-}
{-# language TemplateHaskell #-}

module Control.Concurrent.Synchronized
  ( synchronized
  ) where

import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (bracket_)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import GHC.Prim (unsafeCoerce#)
import GHC.Types (Any)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.HashMap.Strict as HashMap
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

-- |
-- @synchronized n action@ executes @action@, allowing at most @n@ threads to
-- execute it concurrently.
--
-- @
-- \$synchronized :: Int -> IO a -> IO a
-- @
synchronized :: TH.Q TH.Exp
synchronized = do
  loc <- mkFastLoc <$> TH.location
  [| synchronized_ loc |]

synchronized_ :: FastLoc -> Int -> IO a -> IO a
synchronized_ loc n act =
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

locksVar :: TVar (HashMap FastLoc Any)
locksVar =
  unsafePerformIO (newTVarIO mempty)
{-# NOINLINE locksVar #-}

newAny :: FastLoc -> a -> IO a
newAny loc val =
  atomically $ do
    locks <- readTVar locksVar
    case HashMap.lookup loc locks of
      Nothing -> do
        writeTVar locksVar (HashMap.insert loc (unsafeCoerce# val) locks)
        pure (unsafeCoerce# val)
      Just lock ->
        pure (unsafeCoerce# lock)

lookupAny :: FastLoc -> IO (Maybe a)
lookupAny loc =
  atomically (unsafeCoerce# . HashMap.lookup loc <$> readTVar locksVar)

-- Like 'Loc', but with a faster 'Hashable' instance.
data FastLoc
  = FastLoc
      {-# UNPACK #-} !Text
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  deriving (Eq, Generic, Show)

instance Hashable FastLoc

instance TH.Lift FastLoc where
  lift (FastLoc s x y) =
    [| FastLoc (pack $(TH.lift (unpack s))) x y |]

mkFastLoc :: TH.Loc -> FastLoc
mkFastLoc = \case
  TH.Loc x y z (n, m) _ ->
    FastLoc (pack x <> pack y <> pack z) n m
