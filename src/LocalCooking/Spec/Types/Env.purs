module LocalCooking.Spec.Types.Env where

import LocalCooking.Common.User.Password (HashedPassword)
import Facebook.Types (FacebookClientId)
import Google.ReCaptcha (ReCaptchaSiteKey)



-- FIXME why is preliminary in env? It should only exist once during boot
type Env =
  { development            :: Boolean
  , facebookClientId       :: FacebookClientId
  , googleReCaptchaSiteKey :: ReCaptchaSiteKey
  , salt                   :: HashedPassword
  }
