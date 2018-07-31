module LocalCooking.Spec.FormData where

import LocalCooking.Semantics.Common (SocialLoginForm)

import Prelude
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?))
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


data FacebookLoginUnsavedFormData
  = FacebookLoginUnsavedFormDataRegister
    { email :: String
    , emailConfirm :: String
    , socialLogin :: SocialLoginForm
    }
  | FacebookLoginUnsavedFormDataSecurity
    { email :: String
    , emailConfirm :: String
    , socialLogin :: SocialLoginForm
    }

derive instance genericFacebookLoginUnsavedFormData :: Generic FacebookLoginUnsavedFormData

instance showFacebookLoginUnsavedFormData :: Show FacebookLoginUnsavedFormData where
  show = gShow

instance eqFacebookLoginUnsavedFormData :: Eq FacebookLoginUnsavedFormData where
  eq = gEq

instance arbitraryFacebookLoginUnsavedFormData :: Arbitrary FacebookLoginUnsavedFormData where
  arbitrary = oneOf $ NonEmpty
    ( do email <- arbitrary
         emailConfirm <- arbitrary
         socialLogin <- arbitrary
         pure $ FacebookLoginUnsavedFormDataRegister {email,emailConfirm,socialLogin}
    )
    [ do email <- arbitrary
         emailConfirm <- arbitrary
         socialLogin <- arbitrary
         pure $ FacebookLoginUnsavedFormDataSecurity {email,emailConfirm,socialLogin}
    ]

instance encodeJsonFacebookLoginUnsavedFormData :: EncodeJson FacebookLoginUnsavedFormData where
  encodeJson x = case x of
    FacebookLoginUnsavedFormDataRegister {email,emailConfirm,socialLogin}
      -> "register" :=
         ( "email" := email
         ~> "emailConfirm" := emailConfirm
         ~> "socialLogin" := socialLogin
         ~> jsonEmptyObject )
      ~> jsonEmptyObject
    FacebookLoginUnsavedFormDataSecurity {email,emailConfirm,socialLogin}
      -> "security" :=
         ( "email" := email
         ~> "emailConfirm" := emailConfirm
         ~> "socialLogin" := socialLogin
         ~> jsonEmptyObject )
      ~> jsonEmptyObject

instance decodeJsonFacebookLoginUnsavedFormData :: DecodeJson FacebookLoginUnsavedFormData where
  decodeJson json = do
    o <- decodeJson json
    let register = do
          o' <- o .? "register"
          email <- o' .? "email"
          emailConfirm <- o' .? "emailConfirm"
          socialLogin <- o' .? "socialLogin"
          pure (FacebookLoginUnsavedFormDataRegister {email,emailConfirm,socialLogin})
        security = do
          o' <- o .? "security"
          email <- o' .? "email"
          emailConfirm <- o' .? "emailConfirm"
          socialLogin <- o' .? "socialLogin"
          pure (FacebookLoginUnsavedFormDataSecurity {email,emailConfirm,socialLogin})
    register <|> security
