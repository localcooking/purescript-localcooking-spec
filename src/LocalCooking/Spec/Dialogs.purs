module LocalCooking.Spec.Dialogs where

import LocalCooking.Spec.Dialogs.Login (loginDialog)
import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Semantics.Common (Login)
import LocalCooking.Global.Error (GlobalError)
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks)
import LocalCooking.Dependencies.Validate
  (PasswordVerifyUnauthSparrowClientQueues)

import Prelude
import Data.Maybe (Maybe)
import Data.UUID (GENUUID)
import Data.URI.Location (class ToLocation)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import React as R
import DOM (DOM)
import Crypto.Scrypt (SCRYPT)

import Queue (WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO


type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , scrypt    :: SCRYPT
  , dom       :: DOM
  | eff)



dialogs :: forall eff siteLinks userDetails userDetailsLinks
         . LocalCookingSiteLinks siteLinks userDetailsLinks
        => ToLocation siteLinks
        => LocalCookingParams siteLinks userDetails (Effects eff)
        -> { dialogQueues ::
             { login ::
               { openQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Login)
               , closeQueue :: One.Queue (write :: WRITE) (Effects eff) Unit
               }
             }
           , dependencies ::
             { passwordVerifyUnauthQueues :: PasswordVerifyUnauthSparrowClientQueues (Effects eff)
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
  ]
