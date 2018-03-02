{-# language LambdaCase      #-}
{-# language RankNTypes      #-}
{-# language TemplateHaskell #-}

module Control.Concurrent.Synchronized
  ( synchronized
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Monoid ((<>))
import Data.Text.Short (ShortText, fromString, toString)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.HashTable.IO as HashTable
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

type HashTable k v
  = HashTable.BasicHashTable k v

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
  HashTable.lookup locksTable loc >>= \case
    Just with ->
      appWith with act
    Nothing ->
      createWith loc n >>= \with -> appWith with act

createWith :: FastLoc -> Int -> IO With
createWith loc n =
  withMVar createVar $ \_ -> do
    HashTable.lookup locksTable loc >>= \case
      Nothing -> do
        with <-
          if n <= 1
            then do
              lock <- newMVar ()
              pure (With (withMVar lock . const))
            else do
              lock <- newQSem n
              pure (With (bracket_ (waitQSem lock) (signalQSem lock)))
        HashTable.insert locksTable loc with
        pure with
      Just with ->
        pure with

-- Top-level MVar that only allows one thread to insert into the global lock
-- table at a time. This only happens on the first call to each unique
-- 'synchronized', so there should be almost no contention.
createVar :: MVar ()
createVar =
  unsafePerformIO (newMVar ())
{-# NOINLINE createVar #-}

locksTable :: HashTable FastLoc With
locksTable =
  unsafePerformIO HashTable.new
{-# NOINLINE locksTable #-}

newtype With
  = With { appWith :: forall a. IO a -> IO a }

-- Like 'Loc', but with a faster 'Hashable' instance.
data FastLoc
  = FastLoc
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !ShortText
  deriving Eq

instance Hashable FastLoc where
  hashWithSalt n (FastLoc x y s) =
    hashWithSalt n x `hashWithSalt` y `hashWithSalt` s

instance TH.Lift FastLoc where
  lift (FastLoc x y s) =
    [| FastLoc x y (fromString $(TH.lift (toString s))) |]

mkFastLoc :: TH.Loc -> FastLoc
mkFastLoc = \case
  TH.Loc x y z (n, m) _ ->
    FastLoc n m (fromString x <> fromString y <> fromString z)
