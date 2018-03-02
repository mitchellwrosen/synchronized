{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}

module Control.Concurrent.Synchronized.Internal
  ( Loc
  , location
  ) where

import Data.Hashable (Hashable)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

location :: TH.Q TH.Exp
location =
  TH.lift . mkLoc =<< TH.location

mkLoc :: TH.Loc -> Loc
mkLoc = \case
  TH.Loc x y z (n, m) _ ->
    Loc (pack x <> pack y <> pack z) n m

data Loc
  = Loc {-# UNPACK #-} !Text {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Generic)

instance Hashable Loc

instance TH.Lift Loc where
  lift (Loc s x y) =
    [| Loc (pack $(TH.lift (unpack s))) x y |]
