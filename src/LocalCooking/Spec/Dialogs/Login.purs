module LocalCooking.Spec.Dialogs.Login where

import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Spec.Misc.Social (mkSocialFab)
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Global.Error
  ( GlobalError (GlobalErrorAuthFailure, GlobalErrorCode)
  , AuthTokenFailure (AuthLoginFailure, AuthTokenInternalError)
  , ErrorCode (UserUserDoesntExist)
  )
import LocalCooking.Dependencies.Validate (PasswordVerifyUnauthSparrowClientQueues)
import LocalCooking.Semantics.Validate (PasswordVerifyUnauth (..))
import LocalCooking.Semantics.User (UserExists (..))
import LocalCooking.Semantics.Common (Login (..))
import LocalCooking.Global.Links.External (ThirdPartyLoginReturnLinks (..))
import LocalCooking.Global.Links.Class (registerLink, class LocalCookingSiteLinks)
import Facebook.Call (FacebookLoginLink (..))
import Facebook.State (FacebookLoginState (..))
import Components.Dialog.Generic (genericDialog)
import Components.Form.Email as Email
import Components.Form.Password as Password

import Prelude
import Data.Password (hashPassword)
import Data.Maybe (Maybe (..))
import Data.URI.URI (print) as URI
import Data.URI.Location (class ToLocation, toLocation)
import Data.UUID (genUUID, GENUUID)
import Control.Monad.Base (liftBase)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)

import React (ReactElement) as R
import React.DOM (div, text) as R
import React.DOM.Props as RP
import React.DOM.Props.PreventDefault (preventDefault)
import React.Icons (facebookIcon, twitterIcon, googleIcon)
import DOM (DOM)

import MaterialUI.Button (button)
import MaterialUI.Button as Button
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


loginDialog :: forall eff siteLinks userDetails userDetailsLinks
             . LocalCookingSiteLinks siteLinks userDetailsLinks
            => Eq siteLinks
            => ToLocation siteLinks
            => LocalCookingParams siteLinks userDetails (Effects eff)
            -> { loginDialogQueue           :: OneIO.IOQueues (Effects eff) Unit (Maybe Login)
               , loginCloseQueue            :: One.Queue (write :: WRITE) (Effects eff) Unit
               , passwordVerifyUnauthQueues :: PasswordVerifyUnauthSparrowClientQueues (Effects eff)
               , globalErrorQueue           :: One.Queue (write :: WRITE) (Effects eff) GlobalError
               , env                        :: Env
               }
            -> R.ReactElement
loginDialog
  params@{toURI,currentPageSignal,siteLinks}
  { loginDialogQueue
  , loginCloseQueue
  , passwordVerifyUnauthQueues
  , globalErrorQueue
  , env: {facebookClientId, salt}
  } =
  genericDialog
  { dialogQueue: loginDialogQueue
  , closeQueue: Just loginCloseQueue
  , dialogSignal: Nothing
  , buttons: \{close} ->
    [ button
      { color: Button.secondary
      , onClick: mkEffFn1 preventDefault
      , onTouchTap: mkEffFn1 \e -> do
          preventDefault e
          unsafeCoerceEff $ do
            close
            siteLinks registerLink
      , href: URI.print $ toURI $ toLocation $ registerLink :: siteLinks
      } [R.text "Register"]
    ]
  , title: \_ -> "Login"
  , submitValue: Just "Submit"
  , extraOnClose: pure unit
  , pends: true
  , content:
    { component: \{submitDisabled} ->
      let _ = unsafePerformEff $ do
            k <- show <$> genUUID
            let submitValue = do
                  mEmail <- IxSignal.get emailSignal
                  x <- case mEmail of
                    Email.EmailGood _ -> do
                      p1 <- IxSignal.get passwordSignal
                      pure (p1 == "")
                    _ -> pure true
                  submitDisabled x
            IxQueue.onIxQueue emailQueue k \_ -> submitValue
            IxQueue.onIxQueue passwordQueue k \_ -> submitValue
            IxSignal.subscribe (\_ -> submitValue) emailSignal
            IxSignal.subscribe (\_ -> submitValue) passwordSignal
      in  [ Email.email
            { label: R.text "Email"
            , fullWidth: true
            , name: "login-email"
            , id: "login-email"
            , emailSignal: emailSignal
            , parentSignal: Nothing
            , updatedQueue: emailQueue
            , setQueue
            }
          , Password.password
            { label: R.text "Password"
            , fullWidth: true
            , name: "login-password"
            , id: "login-password"
            , passwordSignal: passwordSignal
            , parentSignal: Nothing
            , updatedQueue: passwordQueue
            , errorQueue: passwordErrorQueue
            }
          , R.div [RP.style {display: "flex", justifyContent: "space-evenly", paddingTop: "2em"}] $
              [ mkSocialFab facebookClientId "#3b5998" "#1e3f82" facebookIcon false $ Just $ FacebookLoginLink
                { redirectURL: toURI (toLocation FacebookLoginReturn)
                , state: FacebookLoginState
                  { origin: toLocation $ unsafePerformEff $ IxSignal.get currentPageSignal
                  , formData: Nothing
                  }
                }
              , mkSocialFab facebookClientId "#1da1f3" "#0f8cdb" twitterIcon false Nothing
              , mkSocialFab facebookClientId "#dd4e40" "#c13627" googleIcon false Nothing
              ]
          ]
    , obtain: \_ -> do
      mEmail <- liftEff (IxSignal.get emailSignal)
      case mEmail of
        Email.EmailGood email -> do
          pw <- liftEff (IxSignal.get passwordSignal)
          hashedPassword <- liftBase (hashPassword {salt: salt, password: pw})
          mVerify <- OneIO.callAsync
            passwordVerifyUnauthQueues
            (PasswordVerifyUnauth {email,password: hashedPassword})
          case mVerify of
            Just mExists -> liftEff $ case mExists of
              UserDoesntExist -> do
                One.putQueue globalErrorQueue (GlobalErrorCode UserUserDoesntExist)
                pure Nothing
              UserExists isCorrect ->
                if isCorrect
                  then pure $ Just $ Login {email,password: hashedPassword}
                  else do
                    One.putQueue globalErrorQueue (GlobalErrorAuthFailure AuthLoginFailure)
                    One.putQueue passwordErrorQueue unit
                    pure Nothing
            Nothing -> do
              liftEff $ do
                One.putQueue globalErrorQueue (GlobalErrorAuthFailure AuthTokenInternalError)
                One.putQueue passwordErrorQueue unit
              pure Nothing
        _ -> do
          -- liftEff $ log "bad email!" -- FIXME bug out somehow?
          pure Nothing
    , reset: do
      IxSignal.set (Email.EmailPartial "") emailSignal
      IxSignal.set "" passwordSignal
    }
  , windowSizeSignal: params.windowSizeSignal
  }
  where
    emailSignal = unsafePerformEff $ IxSignal.make $ Email.EmailPartial ""
    passwordSignal = unsafePerformEff $ IxSignal.make ""
    emailQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    passwordQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    passwordErrorQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    setQueue = unsafePerformEff $ writeOnly <$> One.newQueue
