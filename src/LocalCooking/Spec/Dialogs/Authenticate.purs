module LocalCooking.Spec.Dialogs.Authenticate where

import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Global.Error
  ( GlobalError (GlobalErrorAuthFailure, GlobalErrorCode)
  , AuthTokenFailure (AuthLoginFailure, AuthTokenInternalError)
  , ErrorCode (UserUserDoesntExist))
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks)
import LocalCooking.Dependencies.Validate (PasswordVerifySparrowClientQueues)
import LocalCooking.Semantics.User (UserExists (..))
import Components.Dialog.Generic (genericDialog)
import Components.Form.Password as Password

import Prelude
import Data.Password (HashedPassword, hashPassword)
import Data.Maybe (Maybe (..))
import Data.UUID (genUUID, GENUUID)
import Data.URI.Location (class ToLocation)
import Data.Argonaut.JSONTuple (JSONTuple (..))
import Control.Monad.Base (liftBase)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)

import React (ReactElement) as R
import React.DOM (text) as R
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
                   => Eq siteLinks
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
  { dialogQueue: authenticateDialogQueue
  , closeQueue: Nothing
  , dialogSignal: Nothing
  , buttons: \_ -> []
  , extraOnClose: pure unit
  , title: \_ -> "Authenticate"
  , submitValue: Just "Submit"
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
    , obtain: \_ -> do
      mAuthToken <- liftEff (IxSignal.get authTokenSignal)
      case mAuthToken of
        Nothing -> pure Nothing
        Just authToken -> do
          pw <- liftEff (IxSignal.get passwordSignal)
          hashedPassword <- liftBase (hashPassword {salt: env.salt, password: pw})
          mVerify <- OneIO.callAsync
            passwordVerifyQueues
            (JSONTuple authToken hashedPassword)
          case mVerify of
            Just mExists -> liftEff $ case mExists of
              UserDoesntExist -> do
                One.putQueue globalErrorQueue (GlobalErrorCode UserUserDoesntExist)
                pure Nothing
              UserExists isCorrect ->
                if isCorrect
                  then pure (Just hashedPassword)
                  else do
                    One.putQueue globalErrorQueue (GlobalErrorAuthFailure AuthLoginFailure)
                    One.putQueue passwordErrorQueue unit
                    pure Nothing
            Nothing -> do
              liftEff $ do
                One.putQueue globalErrorQueue (GlobalErrorAuthFailure AuthTokenInternalError)
                One.putQueue passwordErrorQueue unit
              pure Nothing
    , reset: do
      IxSignal.set "" passwordSignal
    }
  , windowSizeSignal: params.windowSizeSignal
  }
  where
    passwordSignal = unsafePerformEff $ IxSignal.make ""
    passwordQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    passwordErrorQueue = unsafePerformEff $ writeOnly <$> One.newQueue
