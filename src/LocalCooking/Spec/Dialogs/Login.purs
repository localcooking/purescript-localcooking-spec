module LocalCooking.Spec.Dialogs.Login where

import LocalCooking.Spec.Dialogs.Generic (genericDialog)
import LocalCooking.Spec.Common.Form.Email as Email
import LocalCooking.Spec.Common.Form.Password as Password
import LocalCooking.Spec.Types.Env (Env)
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
import Control.Monad.Eff.Console (CONSOLE, log)

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
  , console   :: CONSOLE
  , dom       :: DOM
  | eff)


loginDialog :: forall eff siteLinks userDetails userDetailsLinks
             . LocalCookingSiteLinks siteLinks userDetailsLinks
            => ToLocation siteLinks
            => LocalCookingParams siteLinks userDetails (Effects eff)
            -> { loginDialogQueue     :: OneIO.IOQueues (Effects eff) Unit (Maybe Login)
               , loginCloseQueue      :: One.Queue (write :: WRITE) (Effects eff) Unit
               , passwordVerifyUnauthQueues :: PasswordVerifyUnauthSparrowClientQueues (Effects eff)
               , globalErrorQueue     :: One.Queue (write :: WRITE) (Effects eff) GlobalError
               , env                  :: Env
               , toRegister           :: Eff (Effects eff) Unit
               }
            -> R.ReactElement
loginDialog
  params@{toURI,currentPageSignal}
  { loginDialogQueue
  , loginCloseQueue
  , passwordVerifyUnauthQueues
  , globalErrorQueue
  , env: {facebookClientId, salt}
  , toRegister
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
            toRegister
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
              [ mkFab facebookClientId "#3b5998" "#1e3f82" facebookIcon $ Just $ FacebookLoginLink
                { redirectURL: toURI (toLocation FacebookLoginReturn)
                , state: FacebookLoginState
                  { origin: toLocation $ unsafePerformEff $ IxSignal.get currentPageSignal
                  , formData: Nothing
                  }
                }
              , mkFab facebookClientId "#1da1f3" "#0f8cdb" twitterIcon Nothing
              , mkFab facebookClientId "#dd4e40" "#c13627" googleIcon Nothing
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
          case mVerify of -- FIXME nonexistent auth token error message?
            Just JSONUnit -> do
              pure $ Just $ Login {email,password: hashedPassword} -- FIXME delay until other queues are finished - user details, auth token, etc.
            _ -> do
              -- liftEff $ case mVerify of
              --   Nothing ->
              --     One.putQueue globalErrorQueue (SnackbarMessageAuthFailure AuthExistsFailure)
              --   _ ->
              liftEff $ do
                One.putQueue globalErrorQueue (GlobalErrorAuthFailure AuthTokenLoginFailure)
                One.putQueue passwordErrorQueue unit
              pure Nothing
        _ -> do
          liftEff $ log "bad email!" -- FIXME bug out somehow?
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


-- | For social logins
mkFab :: FacebookClientId -> String -> String -> R.ReactElement
      -> Maybe FacebookLoginLink -> R.ReactElement
mkFab facebookClientId mainColor darkColor icon mLink =
  Button.withStyles
    (\theme ->
      { root: createStyles
        { backgroundColor: mainColor
        , color: "#ffffff"
        , "&:hover": {backgroundColor: darkColor}
        }
      }
    )
    (\{classes} ->
      button
        { variant: Button.fab
        , classes: Button.createClasses {root: classes.root}
        , disabled: case mLink of
          Nothing -> true
          _ -> false
        , href: case mLink of
          Nothing -> ""
          Just link -> URI.print $
            facebookLoginLinkToURI facebookClientId link
        } [icon]
    )
