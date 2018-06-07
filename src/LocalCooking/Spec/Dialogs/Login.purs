module LocalCooking.Spec.Dialogs.Login where

import LocalCooking.Spec.Dialogs.Generic (genericDialog)
import LocalCooking.Spec.Common.Form.Email as Email
import LocalCooking.Spec.Common.Form.Password as Password
import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Spec.Misc.Social (mkSocialFab)
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Global.Error
  ( GlobalError (GlobalErrorAuthFailure)
  , AuthTokenFailure (AuthTokenLoginFailure))
import LocalCooking.Dependencies.Validate (PasswordVerifyUnauthSparrowClientQueues, PasswordVerifyUnauth (..))
import LocalCooking.Global.Links.External (ThirdPartyLoginReturnLinks (..))
import LocalCooking.Global.Links.Class (registerLink, class LocalCookingSiteLinks)
import LocalCooking.Common.User.Password (hashPassword)
import LocalCooking.Semantics.Common (Login (..))
import Facebook.Types (FacebookClientId)
import Facebook.Call (FacebookLoginLink (..), facebookLoginLinkToURI)
import Facebook.State (FacebookLoginState (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.URI.URI (print) as URI
import Data.URI.Location (class ToLocation, toLocation)
import Data.UUID (genUUID, GENUUID)
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Control.Monad.Base (liftBase)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)

import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.DOM.Props.PreventDefault (preventDefault)
import React.Icons (facebookIcon, twitterIcon, googleIcon)
import DOM (DOM)

import MaterialUI.Types (createStyles)
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
  params
  { dialogQueue: loginDialogQueue
  , closeQueue: Just loginCloseQueue
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
  , title: "Login"
  , submitValue: "Submit"
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
    , obtain: do
      mEmail <- liftEff (IxSignal.get emailSignal)
      case mEmail of
        Email.EmailGood email -> do
          pw <- liftEff (IxSignal.get passwordSignal)
          hashedPassword <- liftBase (hashPassword {salt: salt, password: pw})
          mVerify <- OneIO.callAsync
            passwordVerifyUnauthQueues
            (PasswordVerifyUnauth {email,password: hashedPassword})
          case mVerify of
            Just JSONUnit -> do
              pure $ Just $ Login {email,password: hashedPassword}
            Nothing -> do
              liftEff $ do
                One.putQueue globalErrorQueue (GlobalErrorAuthFailure AuthTokenLoginFailure)
                One.putQueue passwordErrorQueue unit
              pure Nothing
        _ -> do
          -- liftEff $ log "bad email!" -- FIXME bug out somehow?
          pure Nothing
    , reset: do
      IxSignal.set (Email.EmailPartial "") emailSignal
      IxSignal.set "" passwordSignal
    }
  }
  where
    emailSignal = unsafePerformEff $ IxSignal.make $ Email.EmailPartial ""
    passwordSignal = unsafePerformEff $ IxSignal.make ""
    emailQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    passwordQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    passwordErrorQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    setQueue = unsafePerformEff $ writeOnly <$> One.newQueue
