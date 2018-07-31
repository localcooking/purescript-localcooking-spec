module LocalCooking.Spec.Content.Register where

import LocalCooking.Spec.Misc.Social (mkSocialLogin)
import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Spec.FormData
  (FacebookLoginUnsavedFormData (FacebookLoginUnsavedFormDataRegister))
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Global.Error (GlobalError (GlobalErrorRegister))
import LocalCooking.Dependencies.Common (RegisterSparrowClientQueues)
import LocalCooking.Semantics.Common (Register (..), SocialLoginForm (..))
import Google.ReCaptcha (ReCaptchaResponse)
import Components.Misc.Pending (pending)
import Components.Form.Email (EmailState (..), email) as Email
import Components.Form.Password as Password
import Components.Form.Submit as Submit
import Components.Form.ReCaptcha (reCaptcha)

import Prelude
import Data.Password (hashPassword)
import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Data.URI.Location (class ToLocation)
import Text.Email.Validate (toString) as Email
import Control.Monad.Base (liftBase)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Timer (TIMER)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (br, div, text) as R
import React.DOM.Props as RP
import React.Queue.WhileMounted as Queue

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Divider (divider)
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid
import MaterialUI.Button as Button
import Crypto.Scrypt (SCRYPT)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue.Types (readOnly, writeOnly, allowWriting, allowReading)
import Queue (WRITE, READ)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxQueue (IxQueue)
import IxQueue as IxQueue


-- TODO bind to local cooking args
type State =
  { rerender :: Unit
  , socialLogin :: SocialLoginForm
  }

initialState :: {initSocialLogin :: SocialLoginForm} -> State
initialState {initSocialLogin} =
  { rerender: unit
  , socialLogin: initSocialLogin
  }

data Action
  = SubmitRegister
  | ReRender
  | ClickedPrivacyPolicy
  | ReceivedUnsavedFormData RegisterUnsavedFormData


type Effects eff =
  ( ref       :: REF
  , scrypt    :: SCRYPT
  , exception :: EXCEPTION
  , uuid      :: GENUUID
  , timer     :: TIMER
  | eff)


spec :: forall eff siteLinks userDetails
      . ToLocation siteLinks
     => LocalCookingParams siteLinks userDetails (Effects eff)
     -> { registerQueues     :: RegisterSparrowClientQueues (Effects eff)
        , globalErrorQueue   :: One.Queue (write :: WRITE) (Effects eff) GlobalError
        , privacyPolicyQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Unit)
        , env                :: Env
        , email ::
          { signal        :: IxSignal (Effects eff) Email.EmailState
          , updatedQueue  :: IxQueue (read :: READ) (Effects eff) Unit
          , setQueue      :: One.Queue (write :: WRITE) (Effects eff) Email.EmailState
          }
        , emailConfirm ::
          { signal        :: IxSignal (Effects eff) Email.EmailState
          , updatedQueue  :: IxQueue (read :: READ) (Effects eff) Unit
          , setQueue      :: One.Queue (write :: WRITE) (Effects eff) Email.EmailState
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
  params
  { registerQueues
  , globalErrorQueue
  , privacyPolicyQueue
  , env
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
          Nothing -> pure unit -- liftEff $ log "Privacy policy denied?"
          Just _ -> liftEff $ do
            IxSignal.set true privacy.disabledSignal
            IxQueue.broadcastIxQueue (allowWriting password.updatedQueue) unit
      ReceivedUnsavedFormData (RegisterUnsavedFormData xs@{socialLogin}) -> do
        liftEff $ do
          One.putQueue email.setQueue (Email.EmailPartial xs.email)
          One.putQueue emailConfirm.setQueue (Email.EmailPartial xs.emailConfirm)
        void $ T.cotransform _ {socialLogin = socialLogin}
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
                  OneIO.callAsync registerQueues (Register {email,password,reCaptcha,social: state.socialLogin})
                liftEff $ do
                  One.putQueue globalErrorQueue $ GlobalErrorRegister mErr
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
            , setQueue: email.setQueue
            }
          , Email.email
            { label: R.text "Email Confirm"
            , fullWidth: true
            , name: "register-email-confirm"
            , id: "register-email-confirm"
            , emailSignal: emailConfirm.signal
            , parentSignal: Just email.signal
            , updatedQueue: emailConfirm.updatedQueue
            , setQueue: emailConfirm.setQueue
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
              mkSocialLogin params
                { env
                , getUnsavedFormData: do
                    email' <- IxSignal.get email.signal
                    emailConfirm' <- IxSignal.get emailConfirm.signal
                    pure $ FacebookLoginUnsavedFormDataRegister
                      { email: case email' of
                          Email.EmailPartial e -> e
                          Email.EmailBad e -> e
                          Email.EmailGood e -> Email.toString e
                      , emailConfirm: case emailConfirm' of
                          Email.EmailPartial e -> e
                          Email.EmailBad e -> e
                          Email.EmailGood e -> Email.toString e
                      , socialLogin: state.socialLogin
                      }
                }
                state.socialLogin
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
            , fullWidth: false
            } [R.text "Privacy Policy"]
          , R.br [] []
          , Submit.submit
            { color: Button.secondary
            , variant: Button.raised
            , size: Button.large
            , style: createStyles {marginTop: "1em"}
            , disabledSignal: submit.disabledSignal
            , triggerQueue: submit.queue
            , fullWidth: false
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



newtype RegisterUnsavedFormData = RegisterUnsavedFormData
  { email :: String
  , emailConfirm :: String
  , socialLogin :: SocialLoginForm
  }



register :: forall eff siteLinks userDetails
          . ToLocation siteLinks
         => LocalCookingParams siteLinks userDetails (Effects eff)
         -> { registerQueues       :: RegisterSparrowClientQueues (Effects eff)
            , globalErrorQueue     :: One.Queue (write :: WRITE) (Effects eff) GlobalError
            , privacyPolicyQueue   :: OneIO.IOQueues (Effects eff) Unit (Maybe Unit)
            , env                  :: Env
            , unsavedFormDataQueue :: One.Queue (write :: WRITE) (Effects eff) RegisterUnsavedFormData
            }
         -> R.ReactElement
register
  params
  { registerQueues
  , globalErrorQueue
  , privacyPolicyQueue
  , env
  , unsavedFormDataQueue
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            params
            { registerQueues
            , globalErrorQueue
            , privacyPolicyQueue
            , env
            , email:
              { signal: emailSignal
              , updatedQueue: emailUpdatedQueue
              , setQueue: emailSetQueue
              }
            , emailConfirm:
              { signal: emailConfirmSignal
              , updatedQueue: emailConfirmUpdatedQueue
              , setQueue: emailConfirmSetQueue
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
            } ) (initialState {initSocialLogin: socialLogin})
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
        $ Queue.whileMountedOne
            (allowReading unsavedFormDataQueue)
            (\this x -> unsafeCoerceEff $ dispatcher this $ ReceivedUnsavedFormData x)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
  where
    emailSignal = unsafePerformEff $ IxSignal.make $ Email.EmailPartial "" 
    emailConfirmSignal = unsafePerformEff $ IxSignal.make $ Email.EmailPartial ""
    socialLogin = SocialLoginForm {fb: Nothing}
    emailUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    emailConfirmUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    emailSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    emailConfirmSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
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
