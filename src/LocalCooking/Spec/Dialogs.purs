module LocalCooking.Spec.Dialogs where

import LocalCooking.Spec.Dialogs.Login (loginDialog)
import LocalCooking.Spec.Dialogs.Authenticate (authenticateDialog)
import LocalCooking.Spec.Dialogs.PrivacyPolicy (privacyPolicyDialog)
import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Semantics.Common (Login)
import LocalCooking.Global.Error (GlobalError)
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks)
import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Dependencies.Validate
  ( PasswordVerifyUnauthSparrowClientQueues
  , PasswordVerifySparrowClientQueues)

import Prelude
import Data.Maybe (Maybe)
import Data.UUID (GENUUID)
import Data.URI.Location (class ToLocation)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import React as R
import DOM (DOM)
import Crypto.Scrypt (SCRYPT)

import Queue (WRITE)
import Queue.Types (writeOnly)
import Queue.One as One
import Queue.One.Aff as OneIO


type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , scrypt    :: SCRYPT
  , dom       :: DOM
  | eff)



type AllDialogs eff = 
  { login ::
    { openQueue :: OneIO.IOQueues eff Unit (Maybe Login)
    , closeQueue :: One.Queue (write :: WRITE) eff Unit
    }
  , authenticate ::
    { openQueue :: OneIO.IOQueues eff Unit (Maybe HashedPassword)
    }
  , privacyPolicy ::
    { openQueue :: OneIO.IOQueues eff Unit (Maybe Unit)
    }
  }


dialogs :: forall eff siteLinks userDetails userDetailsLinks
         . LocalCookingSiteLinks siteLinks userDetailsLinks
        => ToLocation siteLinks
        => LocalCookingParams siteLinks userDetails (Effects eff)
        -> { dialogQueues :: AllDialogs (Effects eff)
           , dependencies ::
             { passwordVerifyUnauthQueues :: PasswordVerifyUnauthSparrowClientQueues (Effects eff)
             , passwordVerifyQueues :: PasswordVerifySparrowClientQueues (Effects eff)
             }
           , globalErrorQueue :: One.Queue (write :: WRITE) (Effects eff) GlobalError
           , env :: Env
           }
        -> Array R.ReactElement
dialogs params {dialogQueues,dependencies,globalErrorQueue,env} =
  [ loginDialog
    params
    { loginDialogQueue: dialogQueues.login.openQueue
    , loginCloseQueue: dialogQueues.login.closeQueue
    , passwordVerifyUnauthQueues: dependencies.passwordVerifyUnauthQueues
    , globalErrorQueue
    , env
    }
  , authenticateDialog
    params
    { authenticateDialogQueue: dialogQueues.authenticate.openQueue
    , passwordVerifyQueues: dependencies.passwordVerifyQueues
    , globalErrorQueue
    , env
    }
  , privacyPolicyDialog
    params
    { privacyPolicyDialogQueue: dialogQueues.privacyPolicy.openQueue
    }
  ]
