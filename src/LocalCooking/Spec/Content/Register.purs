module LocalCooking.Spec.Content.Register where

import LocalCooking.Spec.Common.Pending (pending)
import LocalCooking.Spec.Common.Form.Email as Email
import LocalCooking.Spec.Common.Form.Password as Password
import LocalCooking.Spec.Common.Form.Submit as Submit
import LocalCooking.Spec.Common.Form.ReCaptcha (reCaptcha)
import LocalCooking.Global.Error (GlobalError (GlobalErrorRegister), RegisterError (..))
import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Global.Links.External (ThirdPartyLoginReturnLinks (..))
-- import LocalCooking.Global.Links.Class (class ToLocation, toLocation)
-- import LocalCooking.Client.Dependencies.Register (RegisterSparrowClientQueues, RegisterInitIn (..), RegisterInitOut (..))
import LocalCooking.Dependencies.Common (RegisterSparrowClientQueues)
import LocalCooking.Semantics.Common (Register (..))
import LocalCooking.Common.User.Password (hashPassword)
import Google.ReCaptcha (ReCaptchaResponse)
import Facebook.Call (FacebookLoginLink (..), facebookLoginLinkToURI)
import Facebook.State (FacebookLoginState (..), FacebookLoginUnsavedFormData (FacebookLoginUnsavedFormDataRegister))
import Facebook.Types (FacebookUserId, FacebookClientId)

import Prelude
import Data.Maybe (Maybe (..), isJust)
import Data.UUID (GENUUID)
import Data.URI.URI (print) as URI
import Data.URI.Location (class ToLocation, toLocation)
import Data.Time.Duration (Milliseconds (..))
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Text.Email.Validate as Email
import Control.Monad.Base (liftBase)
import Control.Monad.Aff (delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, Ref)
-- import Control.Monad.Eff.Ref.Extra (takeRef)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Timer (TIMER)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Queue.WhileMounted as Queue
import React.Icons (facebookIcon, twitterIcon, googleIcon)

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Divider (divider)
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import Crypto.Scrypt (SCRYPT)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue.Types (readOnly, writeOnly, allowWriting)
import Queue (WRITE, READ)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxQueue (IxQueue)
import IxQueue as IxQueue


-- TODO bind to local cooking args
type State =
  { rerender :: Unit
  , fbUserId :: Maybe FacebookUserId
  }

initialState :: {initFbUserId :: Maybe FacebookUserId} -> State
initialState {initFbUserId} =
  { rerender: unit
  , fbUserId: initFbUserId
  }

data Action
  = SubmitRegister
  | ReRender
  | ClickedPrivacyPolicy


type Effects eff =
  ( ref       :: REF
  , scrypt    :: SCRYPT
  , exception :: EXCEPTION
  , uuid      :: GENUUID
  , timer     :: TIMER
  , console   :: CONSOLE
  | eff)


spec :: forall eff siteLinks userDetails
      . ToLocation siteLinks
     => LocalCookingParams siteLinks userDetails (Effects eff)
     -> { registerQueues     :: RegisterSparrowClientQueues (Effects eff)
        , globalErrorQueue   :: One.Queue (write :: WRITE) (Effects eff) GlobalError
        , privacyPolicyQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Unit)
        -- , toRoot             :: Eff (Effects eff) Unit
        , env                :: Env
        , email ::
          { signal        :: IxSignal (Effects eff) Email.EmailState
          , updatedQueue  :: IxQueue (read :: READ) (Effects eff) Unit
          }
        , emailConfirm ::
          { signal        :: IxSignal (Effects eff) Email.EmailState
          , updatedQueue  :: IxQueue (read :: READ) (Effects eff) Unit
          }
        , password ::
          { signal       :: IxSignal (Effects eff) String
          , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          }
        , passwordConfirm ::
          { signal       :: IxSignal (Effects eff) String
          , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          }
        , submit ::
          { queue          :: IxQueue (read :: READ) (Effects eff) Unit
          , disabledSignal :: IxSignal (Effects eff) Boolean
          }
        , privacy ::
          { queue          :: IxQueue (read :: READ) (Effects eff) Unit
          , disabledSignal :: IxSignal (Effects eff) Boolean
          }
        , reCaptchaSignal :: IxSignal (Effects eff) (Maybe ReCaptchaResponse)
          -- FIXME ^
        , pendingSignal   :: IxSignal (Effects eff) Boolean
        } -> T.Spec (Effects eff) State Unit Action
spec
  params@{toURI,currentPageSignal}
  { registerQueues
  , globalErrorQueue
  , privacyPolicyQueue
  -- , toRoot
  , env -- : {facebookClientId, salt, googleReCaptchaSiteKey}
  , reCaptchaSignal
  , pendingSignal
  , email
  , emailConfirm
  , password
  , passwordConfirm
  , submit
  , privacy
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ReRender -> void $ T.cotransform _ {rerender = unit}
      ClickedPrivacyPolicy -> do
        mX <- liftBase (OneIO.callAsync privacyPolicyQueue unit)
        case mX of
          Nothing -> liftEff $ log "Privacy policy denied?"
          Just _ -> liftEff $ do
            IxSignal.set true privacy.disabledSignal
            IxQueue.broadcastIxQueue (allowWriting password.updatedQueue) unit
      SubmitRegister -> do
        liftEff $ IxSignal.set true pendingSignal
        mEmail <- liftEff (IxSignal.get email.signal)
        case mEmail of
          Email.EmailGood email -> do
            mReCaptcha <- liftEff (IxSignal.get reCaptchaSignal)
            case mReCaptcha of
              Just reCaptcha -> do
                passwordString <- liftEff (IxSignal.get password.signal)
                mErr <- liftBase $ do
                  password <- liftBase $ hashPassword
                    { password: passwordString
                    , salt: env.salt
                    }
                  pure Nothing -- FIXME
                  -- OneIO.callAsync registerQueues (Register {email,password,reCaptcha,fbUserId: state.fbUserId})
                liftEff $ do
                  One.putQueue globalErrorQueue $ case mErr of
                    Nothing -> GlobalErrorRegister $
                      Just RegisterErrorEmailInUse -- FIXME bad recaptcha response error?
                    Just JSONUnit -> GlobalErrorRegister Nothing
                  IxSignal.set false pendingSignal
              _ -> pure unit
          _ -> pure unit

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: Typography.display2
        , align: Typography.center
        , color: Typography.primary
        } [R.text "Register"]
      , R.div [RP.style {marginBotton: "1em"}] []
      , divider {}
      , grid
        { spacing: Grid.spacing8
        , container: true
        , justify: Grid.centerJustify
        }
        [ grid
          { xs: 6
          , item: true
          }
          [ Email.email
            { label: R.text "Email"
            , fullWidth: true
            , name: "register-email"
            , id: "register-email"
            , emailSignal: email.signal
            , parentSignal: Nothing
            , updatedQueue: email.updatedQueue
            , setQueue
            }
          , Email.email
            { label: R.text "Email Confirm"
            , fullWidth: true
            , name: "register-email-confirm"
            , id: "register-email-confirm"
            , emailSignal: emailConfirm.signal
            , parentSignal: Just email.signal
            , updatedQueue: emailConfirm.updatedQueue
            , setQueue: setConfirmQueue
            }
          , Password.password
            { label: R.text "Password"
            , fullWidth: true
            , name: "register-password"
            , id: "register-password"
            , passwordSignal: password.signal
            , parentSignal: Nothing
            , updatedQueue: password.updatedQueue
            , errorQueue: passwordErrorQueue
            }
          , Password.password
            { label: R.text "Password Confirm"
            , fullWidth: true
            , name: "register-password-confirm"
            , id: "register-password-confirm"
            , passwordSignal: passwordConfirm.signal
            , parentSignal: Just password.signal
            , updatedQueue: passwordConfirm.updatedQueue
            , errorQueue: passwordConfirmErrorQueue
            }
          , R.div [RP.style {display: "flex", justifyContent: "space-evenly", paddingTop: "2em", paddingBottom: "2em"}] $
              [ mkFab env.facebookClientId "#3b5998" "#1e3f82" facebookIcon (isJust state.fbUserId) $
                  Just $ FacebookLoginLink
                  { redirectURL: toURI (toLocation FacebookLoginReturn)
                  , state: FacebookLoginState
                    { origin: toLocation $ unsafePerformEff $ IxSignal.get currentPageSignal
                    , formData: Just $ FacebookLoginUnsavedFormDataRegister
                      { email: case unsafePerformEff (IxSignal.get email.signal) of
                          Email.EmailPartial e -> e
                          Email.EmailBad e -> e
                          Email.EmailGood e -> Email.toString e
                      , emailConfirm: case unsafePerformEff (IxSignal.get emailConfirm.signal) of
                          Email.EmailPartial e -> e
                          Email.EmailBad e -> e
                          Email.EmailGood e -> Email.toString e
                      , fbUserId: Nothing
                      }
                    }
                  }
              , mkFab env.facebookClientId "#1da1f3" "#0f8cdb" twitterIcon false Nothing
              , mkFab env.facebookClientId "#dd4e40" "#c13627" googleIcon false Nothing
              ]
          , reCaptcha
            { reCaptchaSignal
            , reCaptchaSiteKey: env.googleReCaptchaSiteKey
            }
          , Submit.submit
            { color: Button.default
            , variant: Button.raised
            , size: Button.large
            , style: createStyles {}
            , disabledSignal: privacy.disabledSignal
            , triggerQueue: privacy.queue
            } [R.text "Privacy Policy"]
          , R.br [] []
          , Submit.submit
            { color: Button.secondary
            , variant: Button.raised
            , size: Button.large
            , style: createStyles {marginTop: "1em"}
            , disabledSignal: submit.disabledSignal
            , triggerQueue: submit.queue
            } [R.text "Submit"]
          ]
        ]
      , pending
        { pendingSignal
        }
      ]
      where
        passwordErrorQueue = unsafePerformEff $ writeOnly <$> One.newQueue
        passwordConfirmErrorQueue = unsafePerformEff $ writeOnly <$> One.newQueue
        setQueue = unsafePerformEff $ writeOnly <$> One.newQueue
        setConfirmQueue = unsafePerformEff $ writeOnly <$> One.newQueue


register :: forall eff siteLinks userDetails
          . ToLocation siteLinks
         => LocalCookingParams siteLinks userDetails (Effects eff)
         -> { registerQueues     :: RegisterSparrowClientQueues (Effects eff)
            , globalErrorQueue   :: One.Queue (write :: WRITE) (Effects eff) GlobalError
            , privacyPolicyQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Unit)
            -- , toRoot             :: Eff (Effects eff) Unit
            , env                :: Env
            -- , initFormDataRef    :: Ref (Maybe FacebookLoginUnsavedFormData)
            }
         -> R.ReactElement
register
  params
  { registerQueues
  , globalErrorQueue
  , privacyPolicyQueue
  -- , toRoot
  , env
  -- , initFormDataRef
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            params
            { registerQueues
            , globalErrorQueue
            , privacyPolicyQueue
            -- , toRoot
            , env
            , email:
              { signal: emailSignal
              , updatedQueue: emailUpdatedQueue
              }
            , emailConfirm:
              { signal: emailConfirmSignal
              , updatedQueue: emailConfirmUpdatedQueue
              }
            , password:
              { signal: passwordSignal
              , updatedQueue: passwordUpdatedQueue
              }
            , passwordConfirm:
              { signal: passwordConfirmSignal
              , updatedQueue: passwordConfirmUpdatedQueue
              }
            , submit:
              { queue: submitQueue
              , disabledSignal: submitDisabledSignal
              }
            , privacy:
              { queue: privacyQueue
              , disabledSignal: privacyDisabledSignal
              }
            , reCaptchaSignal
            , pendingSignal
            } ) (initialState {initFbUserId: fbUserId})
      submitValue this = do
        mEmail <- IxSignal.get emailSignal
        confirm <- IxSignal.get emailConfirmSignal
        x <- case mEmail of
          Email.EmailGood _ -> do
            p1 <- IxSignal.get passwordSignal
            if p1 == ""
              then pure true
              else do
                p2 <- IxSignal.get passwordConfirmSignal
                privacyPolicy <- IxSignal.get privacyDisabledSignal
                pure (mEmail /= confirm || p1 /= p2 || not privacyPolicy)
          _ -> pure true
        IxSignal.set x submitDisabledSignal
      reactSpec' =
          Queue.whileMountedIx
            submitQueue
            "onSubmit"
            (\this _ -> unsafeCoerceEff $ dispatcher this SubmitRegister)
        $ Queue.whileMountedIx
            privacyQueue
            "onPrivacy"
            (\this _ -> unsafeCoerceEff $ dispatcher this ClickedPrivacyPolicy)
        $ Queue.whileMountedIx
            emailUpdatedQueue
            "emailUpdated"
            (\this _ -> submitValue this)
        $ Queue.whileMountedIx
            emailConfirmUpdatedQueue
            "emailConfirmUpdated"
            (\this _ -> submitValue this)
        $ Queue.whileMountedIx
            passwordUpdatedQueue
            "passwordUpdated"
            (\this _ -> submitValue this)
        $ Queue.whileMountedIx
            passwordConfirmUpdatedQueue
            "passwordConfirmUpdated"
            (\this _ -> submitValue this)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
  where
    {emailSignal,emailConfirmSignal,fbUserId} = unsafePerformEff $ do
      mX <- pure Nothing -- takeRef initFormDataRef FIXME
      let {email,emailConfirm,fbUserId:fbUserId'} = case mX of
            Just x -> case x of
              FacebookLoginUnsavedFormDataRegister {email,emailConfirm,fbUserId} -> do
                { email: Email.EmailPartial email
                , emailConfirm: Email.EmailPartial emailConfirm
                , fbUserId
                }
              _ ->
                { email: Email.EmailPartial ""
                , emailConfirm: Email.EmailPartial ""
                , fbUserId: Nothing
                }
            _ ->
              { email: Email.EmailPartial ""
              , emailConfirm: Email.EmailPartial ""
              , fbUserId: Nothing
              }
      a <- IxSignal.make email
      b <- IxSignal.make emailConfirm
      unsafeCoerceEff $ log $ "From register: " <> show fbUserId'
      pure {emailSignal: a, emailConfirmSignal: b,fbUserId:fbUserId'}
    emailUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    emailConfirmUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    passwordSignal = unsafePerformEff (IxSignal.make "")
    passwordUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    passwordConfirmSignal = unsafePerformEff (IxSignal.make "")
    passwordConfirmUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    reCaptchaSignal = unsafePerformEff (IxSignal.make Nothing)
    pendingSignal = unsafePerformEff (IxSignal.make false)
    submitQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    submitDisabledSignal = unsafePerformEff (IxSignal.make true)
    privacyQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    privacyDisabledSignal = unsafePerformEff (IxSignal.make false)


-- FIXME use generic
-- | For social logins
mkFab :: FacebookClientId -> String -> String -> R.ReactElement
      -> Boolean -> Maybe FacebookLoginLink -> R.ReactElement
mkFab facebookClientId mainColor darkColor icon hasValue mLink =
  Button.withStyles
    (\theme ->
      { root: createStyles
        { backgroundColor: mainColor
        , color: "#ffffff"
        , "&:hover": {backgroundColor: darkColor}
        }
      }
    )
    \{classes} -> button
      { variant: Button.fab
      , classes: Button.createClasses {root: classes.root}
      , disabled: case mLink of
        Nothing -> true
        _ -> hasValue
      , href: case mLink of
        Nothing -> ""
        Just link -> URI.print (facebookLoginLinkToURI facebookClientId link)
      , style:
        if hasValue
          then createStyles
                { backgroundColor: "#9df860"
                }
          else createStyles {}
      } [icon]
