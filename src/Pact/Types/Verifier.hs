{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pact.Types.Verifier(VerifierArgs(..)) where

import Control.DeepSeq
import Data.Aeson
import Data.Text
import GHC.Generics
import Test.QuickCheck(Arbitrary(..), scale)

import qualified Pact.JSON.Encode as J

import Pact.Types.Orphans()

data VerifierArgs = VerifierArgs
  { _verifierName :: Text
  , _verifierArgs :: [Text]
  }
  deriving (Eq, Show, Generic, Ord)
instance NFData VerifierArgs
instance Arbitrary VerifierArgs where
    arbitrary = VerifierArgs <$> (pack <$> arbitrary) <*> scale (min 10) (fmap pack <$> arbitrary)
instance J.Encode VerifierArgs where
  build va = J.object
    [ "name" J..= _verifierName va
    , "args" J..= J.Array (_verifierArgs va)
    ]

instance FromJSON VerifierArgs where
  parseJSON = withObject "VerifierArgs" $ \o -> do
    name <- o .: "name"
    args <- o .: "args"
    return $ VerifierArgs name args
