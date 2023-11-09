{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Pact.Types.Crypto
-- Copyright   :  (C) 2016 Stuart Popejoy, William Martino
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>, William Martino <will@kadena.io>
--
-- Hashing types and Scheme class.
--
module Pact.Types.Crypto
  ( ST.PPKScheme(..)
  , ST.defPPKScheme
  , SPPKScheme(..)
  , PublicKeyBS(..)
  , PrivateKeyBS(..)
  , SignatureBS(..)
  , sign
  , verifyWebAuthnSig
  , verifyEd25519Sig
  , parseEd25519PubKey
  , parseEd25519SecretKey
  , parseEd25519Signature
  , exportEd25519PubKey
  , exportEd25519SecretKey
  , exportEd25519Signature
  , parseWebAuthnPublicKey
  , exportWebAuthnPublicKey
  , parseWebAuthnSignature
  , exportWebAuthnSignature
  , validCoseSignAlgorithms
  , webAuthnPubKeyHasValidAlg
  , getPublic
  , getPrivate
  , genKeyPair
  , importKeyPair
  -- , Scheme(..)
  , Ed25519KeyPair
  , UserSig(..)
  , WebAuthnSigProvenance(..)
  , WebAuthnPublicKey
  , WebAuthnSignature(..)
  ) where


import Prelude
import GHC.Generics

import qualified Codec.Serialise as Serialise
import Control.Applicative
import Control.Lens
import Control.Monad (unless)
import qualified Crypto.Hash as H
import qualified Pact.Crypto.WebAuthn.Cose.PublicKeyWithSignAlg as WA
import qualified Pact.Crypto.WebAuthn.Cose.SignAlg as WA
import qualified Pact.Crypto.WebAuthn.Cose.Verify as WAVerify
import Data.ByteString    (ByteString)
import Data.ByteString.Short (fromShort)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
-- import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.URL as Base64URL
import Data.String        (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Aeson                        as A
import Control.DeepSeq (NFData)
import Data.Hashable
import Data.Serialize                    as SZ
import qualified Data.Serialize          as S

import Pact.Types.Util
import Pact.Types.Hash
import qualified Pact.Types.Hash as PactHash
import Pact.Types.Scheme               as ST

#ifdef CRYPTONITE_ED25519
import qualified Crypto.Error          as E
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteArray        as B
#else
import "crypto-api" Crypto.Random
import qualified Crypto.Ed25519.Pure as Ed25519
#endif

import qualified Pact.JSON.Encode as J

import Test.QuickCheck
import qualified Test.QuickCheck.Gen as Gen

-- | The type of parsed signatures
data UserSig = ED25519Sig Ed25519.Signature
             | WebAuthnSig WebAuthnSignature WebAuthnSigProvenance
  deriving (Eq, Ord, Show, Generic)

-- | A type for tracking whether a WebAuthn signature was parsed
--   from a string, or an JSON object. This is tracked so that
--   we can fork on the allowed provenance. (Before Pact 4.TODO,
--   only stringified WebAuthn signatures were allowed).
data WebAuthnSigProvenance
  = WebAuthnStringified
  | WebAuthnObject
  deriving (Eq, Ord, Show, Generic)

instance NFData WebAuthnSigProvenance
instance Arbitrary WebAuthnSigProvenance where
  arbitrary = oneof [pure WebAuthnStringified, pure WebAuthnObject]

instance NFData UserSig

instance J.Encode UserSig where
  build (ED25519Sig s) = J.text $ toB16Text $ exportEd25519Signature s
  build (WebAuthnSig (WebAuthnSignature
                      { authenticatorData
                      , signature
                      , clientDataJSON }) WebAuthnObject) = J.object
    [ "authenticatorData" J..= authenticatorData
    , "signature" J..= signature
    , "clientDataJSON" J..= clientDataJSON
    ]
  build (WebAuthnSig sig WebAuthnStringified) = J.text
    (T.decodeUtf8 $ BSL.toStrict $ J.encode sig)
  {-# INLINE build #-}

instance FromJSON UserSig where
  parseJSON x =
    parseWebAuthnObject x <|>
    parseWebAuthnStringified x <|>
    parseEd25519 x
    where
      parseWebAuthnStringified = withText "UserSig" $ \t ->
        case A.decode (BSL.fromStrict $ T.encodeUtf8 t) of
          Nothing -> fail "Could not decode signature"
          Just webauthnSig -> return $ WebAuthnSig webauthnSig WebAuthnStringified
      parseEd25519 = withText "UserSig" $ \t -> do
        dehex <- parseB16Text t
        fmap ED25519Sig $ either fail return $ parseEd25519Signature dehex
      parseWebAuthnObject o = do
        waSig <- parseJSON o
        pure $ WebAuthnSig waSig WebAuthnObject

instance Arbitrary UserSig where
  arbitrary = Gen.oneof
    [ do
      sig <- BS.pack <$> vectorOf 64 arbitrary
      case parseEd25519Signature sig of
        Right parsedSig -> return $ ED25519Sig parsedSig
        Left _ -> error "invalid ed25519 signature"
    , WebAuthnSig <$> (WebAuthnSignature <$> arbitrary <*> arbitrary <*> arbitrary) <*> arbitrary
    ]

-- ed25519

#ifdef CRYPTONITE_ED25519
type Ed25519PrivateKey = Ed25519.SecretKey
#else
type Ed25519PrivateKey = Ed25519.PrivateKey
#endif

verifyEd25519Sig :: Hash -> Ed25519.PublicKey -> Ed25519.Signature -> Either String ()
exportEd25519PubKey :: Ed25519.PublicKey -> ByteString
exportEd25519SecretKey :: Ed25519PrivateKey -> ByteString
exportEd25519Signature :: Ed25519.Signature -> ByteString
parseEd25519PubKey :: ByteString -> Either String Ed25519.PublicKey
parseEd25519SecretKey :: ByteString -> Either String Ed25519PrivateKey
parseEd25519Signature :: ByteString -> Either String Ed25519.Signature

#ifdef CRYPTONITE_ED25519
verifyEd25519Sig (Hash msg) pubKey sig =
  unless (Ed25519.verify pubKey (fromShort msg) sig) $
    Left "invalid ed25519 signature"

exportEd25519PubKey = B.convert
parseEd25519PubKey s = E.onCryptoFailure
  (const $ Left ("Invalid ED25519 Public Key: " ++ show (toB16Text s)))
  Right
  (Ed25519.publicKey s)

exportEd25519SecretKey = B.convert
parseEd25519SecretKey s = E.onCryptoFailure
  (const $ Left ("Invalid ED25519 Private Key: " ++ show (toB16Text s)))
  Right
  (Ed25519.secretKey s)

exportEd25519Signature = B.convert
parseEd25519Signature s = E.onCryptoFailure
  (const $ Left ("Invalid ED25519 Signature: " ++ show (toB16Text s)))
  Right
  (Ed25519.signature s)
#else
type Ed25519PrivateKey = Ed25519.PrivateKey
verifyEd25519Sig (Hash msg) pubKey sig =
  unless (Ed25519.valid (fromShort msg) pubKey sig) $
    Left "invalid ed25519 signature"

exportEd25519PubKey = Ed25519.exportPublic
parseEd25519PubKey s = maybeToEither ("Invalid ED25519 Public Key: " ++ show (toB16Text s))
           (Ed25519.importPublic s)

exportEd25519SecretKey = Ed25519.exportPrivate
parseEd25519SecretKey s = maybeToEither ("Invalid ED25519 Private Key: " ++ show (toB16Text s))
           (Ed25519.importPrivate s)

exportEd25519Signature (Ed25519.Sig bs) = bs
parseEd25519Signature = Right . Ed25519.Sig
#endif

-- webauthn

verifyWebAuthnSig :: Hash -> WA.CosePublicKey -> WebAuthnSignature -> Either String ()
verifyWebAuthnSig
  hsh
  publicKey
  WebAuthnSignature { authenticatorData, signature, clientDataJSON } = do
    -- Enforce that the public key was generated by one of the two most
    -- common signing algorithms. This lowers our susceptibility to
    -- algorithm confusion attacks.
    webAuthnPubKeyHasValidAlg publicKey

    -- Recover the signature, clientData, and authData bytestrings.
    sig <- over _Left ("signature: " <>) $
      Base64.decode (T.encodeUtf8 signature)
    clientData <- over _Left ("clientData: " <>) $
      Base64URL.decode (T.encodeUtf8 clientDataJSON)
    authData <- over _Left ("authData: " <>) $
      Base64.decode (T.encodeUtf8 authenticatorData)

    -- Reconstitute the payload signed by the WebAuthn client.
    let clientDataDigest = B.convert (H.hashWith H.SHA256 clientData)
    let payload = authData <> clientDataDigest

    -- Check the signature's validity.
    over _Left (("webauthn signature check: " <>) . T.unpack) $
      WAVerify.verify publicKey payload sig

    -- Extract the original challenge from client data.
    ClientDataJSON { challenge } <- over _Left ("challenge: " <>) $ A.eitherDecode (BSL.fromStrict clientData)

    -- Check that the input `PactHash` matches the hash of the transaction
    -- that was signed by WebAuthn keys.
    let pactHashText = PactHash.hashToText hsh
    unless (pactHashText == challenge) $
      Left "Hash mismatch, signature signs for the wrong transaction"

parseWebAuthnPublicKey :: ByteString -> Either String WebAuthnPublicKey
parseWebAuthnPublicKey rawPk = do
  pk <- over _Left (\e -> "WebAuthn public key parsing: " <> show e) $
    Serialise.deserialiseOrFail @WA.CosePublicKey (BSL.fromStrict rawPk)
  webAuthnPubKeyHasValidAlg pk
  return pk

webAuthnPubKeyHasValidAlg :: WebAuthnPublicKey -> Either String ()
webAuthnPubKeyHasValidAlg (WA.PublicKeyWithSignAlg _ signAlg) =
  unless (WA.fromCoseSignAlg signAlg `elem` validCoseSignAlgorithms)
    $ Left "Signing algorithm must be EdDSA or P256"

validCoseSignAlgorithms :: [Int]
validCoseSignAlgorithms =
  [ -7 -- ECDSA with SHA-256, the most common WebAuthn signing algorithm.
  , -8 -- EdDSA, which is also supported by YubiKey.
  ]

exportWebAuthnPublicKey :: WebAuthnPublicKey -> ByteString
exportWebAuthnPublicKey = BSL.toStrict . Serialise.serialise

parseWebAuthnSignature :: ByteString -> Either String WebAuthnSignature
parseWebAuthnSignature = A.eitherDecode . BSL.fromStrict

exportWebAuthnSignature :: WebAuthnSignature -> ByteString
exportWebAuthnSignature = J.encodeStrict

--------- SCHEME HELPER DATA TYPES ---------

type Ed25519KeyPair = (Ed25519.PublicKey, Ed25519PrivateKey)

newtype PublicKeyBS = PubBS { _pktPublic :: ByteString }
  deriving (Eq, Generic, Hashable)

instance FromJSON PublicKeyBS where
  parseJSON = withText "PublicKeyBS" $ \s -> do
    s' <- parseB16Text s
    return $ PubBS s'
instance J.Encode PublicKeyBS where
  build (PubBS p) = J.text $ toB16Text p
  {-# INLINE build #-}
instance IsString PublicKeyBS where
  fromString s = case parseB16TextOnly (T.pack s) of
    Left e -> PubBS $ "Bad public key: " <> T.encodeUtf8 (T.pack e)
    Right b -> PubBS b
instance Show PublicKeyBS where
  show (PubBS b) = T.unpack $ toB16Text b

instance FromJSONKey PublicKeyBS where
    fromJSONKey = FromJSONKeyTextParser (either fail (return . PubBS) . parseB16TextOnly)
    {-# INLINE fromJSONKey #-}
instance Arbitrary PublicKeyBS where
  arbitrary = PubBS . BS.pack <$> vector 32


newtype PrivateKeyBS = PrivBS { _pktSecret :: ByteString }
  deriving (Eq, Generic, Hashable)

instance FromJSON PrivateKeyBS where
  parseJSON = withText "PrivateKeyBS" $ \s -> do
    s' <- parseB16Text s
    return $ PrivBS s'
instance J.Encode PrivateKeyBS where
  build (PrivBS p) = J.text $ toB16Text p
  {-# INLINE build #-}
instance IsString PrivateKeyBS where
  fromString s = case parseB16TextOnly (T.pack s) of
    Left e -> PrivBS $ "Bad private key: " <> T.encodeUtf8 (T.pack e)
    Right b -> PrivBS b
instance Show PrivateKeyBS where
  show (PrivBS b) = T.unpack $ toB16Text b
instance Arbitrary PrivateKeyBS where
  arbitrary = PrivBS . BS.pack <$> vector 32

newtype SignatureBS = SigBS ByteString
  deriving (Eq, Show, Generic, Hashable)

instance FromJSON SignatureBS where
  parseJSON = withText "SignatureBS" $ \s -> do
    s' <- parseB16Text s
    return $ SigBS s'
instance J.Encode SignatureBS where
  build (SigBS p) = J.text $ toB16Text p
  {-# INLINE build #-}
instance Arbitrary SignatureBS where
  arbitrary = SigBS . BS.pack <$> vector 64

--------- SCHEME HELPER FUNCTIONS ---------

sign :: Ed25519.PublicKey -> Ed25519PrivateKey -> Hash -> Ed25519.Signature
#ifdef CRYPTONITE_ED25519
sign pub priv (Hash msg) = Ed25519.sign priv pub (fromShort msg)
#else
sign pub priv (Hash msg) = Ed25519.sign (fromShort msg) priv pub
#endif

getPublic :: Ed25519KeyPair -> ByteString
getPublic = exportEd25519PubKey . fst

getPrivate :: Ed25519KeyPair -> ByteString
getPrivate = exportEd25519SecretKey . snd


-- Key Pair setter functions

genKeyPair :: IO (Ed25519.PublicKey, Ed25519PrivateKey)
genKeyPair = ed25519GenKeyPair


-- | Parse a pair of keys (where the public key is optional) into an Ed25519 keypair.
-- Derives Public Key from Private Key if none provided. Trivial in some
-- Crypto schemes (i.e. Elliptic curve ones).
-- Checks that Public Key provided matches the Public Key derived from the Private Key.
importKeyPair :: Maybe PublicKeyBS -> PrivateKeyBS -> Either String Ed25519KeyPair
importKeyPair maybePubBS (PrivBS privBS) = do
  priv <- parseEd25519SecretKey privBS
  let derivedPub = ed25519GetPublicKey priv
  suppliedPub <- case maybePubBS of
    Nothing -> Right Nothing
    Just (PubBS pubBS) -> Just <$> parseEd25519PubKey pubBS

  case suppliedPub of
    Nothing -> return (derivedPub, priv)
    Just pub ->
      if pub == derivedPub
      then return (derivedPub, priv)
      else Left $ "Expected PublicKey "
                ++ show (toB16Text $ exportEd25519PubKey pub)
                ++ " but received "
                ++ show (toB16Text $ exportEd25519PubKey derivedPub)


--------- ED25519 FUNCTIONS AND ORPHANS ---------

#ifdef CRYPTONITE_ED25519
ed25519GenKeyPair :: IO (Ed25519.PublicKey, Ed25519.SecretKey)
ed25519GenKeyPair = do
    secret <- Ed25519.generateSecretKey
    let public = Ed25519.toPublic secret
    return (public, secret)

ed25519GetPublicKey :: Ed25519.SecretKey -> Ed25519.PublicKey
ed25519GetPublicKey = Ed25519.toPublic

instance Ord Ed25519.PublicKey where
  b <= b' = (B.convert b :: ByteString) <= (B.convert b' :: ByteString)
instance Serialize Ed25519.PublicKey where
  put s = S.putByteString (B.convert s :: ByteString)
  get = maybe (fail "Invalid ED25519 Public Key") return =<<
        (E.maybeCryptoError . Ed25519.publicKey <$> S.getByteString 32)


instance Ord Ed25519.SecretKey where
  b <= b' = (B.convert b :: ByteString) <= (B.convert b' :: ByteString)
instance Serialize Ed25519.SecretKey where
  put s = S.putByteString (B.convert s :: ByteString)
  get = maybe (fail "Invalid ED25519 Private Key") return =<<
        (E.maybeCryptoError . Ed25519.secretKey <$> S.getByteString 32)



instance Ord Ed25519.Signature where
  b <= b' = (B.convert b :: ByteString) <= (B.convert b' :: ByteString)
instance Serialize Ed25519.Signature where
  put s = S.put (B.convert s :: ByteString)
  get = maybe (fail "Invalide ED25519 Signature") return =<<
        (E.maybeCryptoError . Ed25519.signature <$> (S.get >>= S.getByteString))
#else
ed25519GenKeyPair :: IO (Ed25519.PublicKey, Ed25519.PrivateKey)
ed25519GenKeyPair = do
    g :: SystemRandom <- newGenIO
    case Ed25519.generateKeyPair g of
      Left _ -> error "Something went wrong in genKeyPairs"
      Right (s,p,_) -> return (p, s)

ed25519GetPublicKey :: Ed25519.PrivateKey -> Ed25519.PublicKey
ed25519GetPublicKey = Ed25519.generatePublic

instance Eq Ed25519.PublicKey where
  b == b' = (Ed25519.exportPublic b) == (Ed25519.exportPublic b')
instance Ord Ed25519.PublicKey where
  b <= b' = (Ed25519.exportPublic b) <= (Ed25519.exportPublic b')
instance Serialize Ed25519.PublicKey where
  put s = S.putByteString (Ed25519.exportPublic s)
  get = maybe (fail "Invalid ED25519 Public Key") return =<<
        (Ed25519.importPublic <$> S.getByteString 32)

instance Eq Ed25519.PrivateKey where
  b == b' = (Ed25519.exportPrivate b) == (Ed25519.exportPrivate b')
instance Ord Ed25519.PrivateKey where
  b <= b' = (Ed25519.exportPrivate b) <= (Ed25519.exportPrivate b')
instance Serialize Ed25519.PrivateKey where
  put s = S.putByteString (Ed25519.exportPrivate s)
  get = maybe (fail "Invalid ED25519 Private Key") return =<<
        (Ed25519.importPrivate <$> S.getByteString 32)

deriving instance Eq Ed25519.Signature
deriving instance Ord Ed25519.Signature
instance Serialize Ed25519.Signature where
  put (Ed25519.Sig s) = S.put s
  get = Ed25519.Sig <$> (S.get >>= S.getByteString)
#endif

type WebAuthnPublicKey = WA.CosePublicKey

-- | This type specifies the format of a WebAuthn signature.
data WebAuthnSignature = WebAuthnSignature
  { clientDataJSON :: T.Text
  , authenticatorData :: T.Text
  , signature :: T.Text
  } deriving (Show, Generic, Eq, Ord)

instance NFData WebAuthnSignature

instance A.FromJSON WebAuthnSignature where
  parseJSON = A.withObject "WebAuthnSignature" $ \o -> do
    clientDataJSON <- o .: "clientDataJSON"
    authenticatorData <- o .: "authenticatorData"
    signature <- o .: "signature"
    pure $ WebAuthnSignature {..}

instance J.Encode WebAuthnSignature where
  build (WebAuthnSignature { clientDataJSON, authenticatorData, signature }) = J.object
    [ "authenticatorData" J..=  authenticatorData
    , "clientDataJSON" J..= clientDataJSON
    , "signature" J..= signature
    ]

-- | This type represents a challenge that was used during
-- a WebAuthn "assertion" flow. For signing Pact payloads, this
-- is the PactHash of a transaction.
newtype ClientDataJSON = ClientDataJSON {
  challenge :: T.Text
  } deriving (Show, Generic)

instance A.FromJSON ClientDataJSON where
  parseJSON = A.withObject "ClientDataJSON" $ \o -> do
    challenge <- o .: "challenge"
    pure $ ClientDataJSON { challenge }
