module LocalCooking.Spec.Dialogs.Authenticate where

import LocalCooking.Spec.Dialogs.Generic (genericDialog)
import LocalCooking.Spec.Common.Form.Password as Password
import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Global.Error (GlobalError (GlobalErrorAuthFailure), AuthTokenFailure (AuthTokenLoginFailure))
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks)
import LocalCooking.Common.User.Password (HashedPassword, hashPassword)
import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn (..))
import LocalCooking.Dependencies.Validate (PasswordVerifySparrowClientQueues)

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (genUUID, GENUUID)
import Data.URI.Location (class ToLocation)
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Control.Monad.Base (liftBase)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)

import React as R
import React.DOM as R
import React.DOM.Props as RP
import DOM (DOM)

import Crypto.Scrypt (SCRYPT)

import Queue.Types (readOnly, writeOnly)
import Queue (WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxQueue as IxQueue
import IxSignal.Internal as IxSignal



type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , scrypt    :: SCRYPT
  , dom       :: DOM
  | eff)


authenticateDialog :: forall eff siteLinks userDetails userDetailsLinks
                    . LocalCookingSiteLinks siteLinks userDetailsLinks
                   => ToLocation siteLinks
                   => LocalCookingParams siteLinks userDetails (Effects eff)
                   -> { authenticateDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe HashedPassword)
                      , passwordVerifyQueues    :: PasswordVerifySparrowClientQueues (Effects eff)
                      , globalErrorQueue        :: One.Queue (write :: WRITE) (Effects eff) GlobalError
                      , env                     :: Env
                      }
                   -> R.ReactElement
authenticateDialog
  params@{authTokenSignal}
  { authenticateDialogQueue
  , passwordVerifyQueues
  , globalErrorQueue
  , env
  } =
  genericDialog
  params
  { dialogQueue: authenticateDialogQueue
  , closeQueue: Nothing
  , buttons: \_ -> []
  , title: "Authenticate"
  , submitValue: "Submit"
  , pends: true
  , content:
    { component: \{submitDisabled} ->
      let _ = unsafePerformEff $ do
            k <- show <$> genUUID
            let submitValue = do
                  p1 <- IxSignal.get passwordSignal
                  submitDisabled (p1 == "")
            IxQueue.onIxQueue passwordQueue k \_ -> submitValue
            IxSignal.subscribe (\_ -> submitValue) passwordSignal
      in  [ Password.password
            { label: R.text "Password"
            , fullWidth: true
            , name: "authenticate-password"
            , id: "authenticate-password"
            , passwordSignal
            , parentSignal: Nothing
            , updatedQueue: passwordQueue
            , errorQueue: passwordErrorQueue
            }
          ]
    , obtain: do
      mAuthToken <- liftEff (IxSignal.get authTokenSignal)
      case mAuthToken of
        Nothing -> pure Nothing
        Just authToken -> do
          pw <- liftEff (IxSignal.get passwordSignal)
          hashedPassword <- liftBase (hashPassword {salt: env.salt, password: pw})
          mVerify <- OneIO.callAsync
            passwordVerifyQueues
            (AccessInitIn {token: authToken, subj: hashedPassword})
          case mVerify of
            Just JSONUnit -> do
              pure (Just hashedPassword)
            Nothing -> do
              liftEff $ do
                One.putQueue globalErrorQueue (GlobalErrorAuthFailure AuthTokenLoginFailure)
                One.putQueue passwordErrorQueue unit
              pure Nothing
    , reset: do
      IxSignal.set "" passwordSignal
    }
  }
  where
    passwordSignal = unsafePerformEff $ IxSignal.make ""
    passwordQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    passwordErrorQueue = unsafePerformEff $ writeOnly <$> One.newQueue