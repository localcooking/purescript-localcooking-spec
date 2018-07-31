module LocalCooking.Spec.Types.Env where

import Facebook.Types (FacebookClientId)
import Google.ReCaptcha (ReCaptchaSiteKey)

import Data.Password (HashedPassword)


-- FIXME why is preliminary in env? It should only exist once during boot
type Env =
  { development            :: Boolean
  , facebookClientId       :: FacebookClientId
  , googleReCaptchaSiteKey :: ReCaptchaSiteKey
  , salt                   :: HashedPassword
  }
