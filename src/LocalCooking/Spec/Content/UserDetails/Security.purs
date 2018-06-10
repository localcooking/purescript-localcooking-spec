module LocalCooking.Spec.Content.UserDetails.Security where

import LocalCooking.Spec.Common.Pending (pending)
import LocalCooking.Spec.Common.Form.Email as Email
import LocalCooking.Spec.Common.Form.Password as Password
import LocalCooking.Spec.Common.Form.Submit as Submit
import LocalCooking.Spec.Misc.Social (mkSocialLogin)
import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Global.Error (GlobalError (GlobalErrorSecurity), SecurityMessage (..))
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Common.User.Password (HashedPassword, hashPassword)
import LocalCooking.Dependencies.Common (GetUserSparrowClientQueues, SetUserSparrowClientQueues)
import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn (..))
import LocalCooking.Semantics.Common (User (..), SetUser (..), SocialLoginForm (..))
-- import LocalCooking.Client.Dependencies.Security (SecuritySparrowClientQueues, SecurityInitIn' (..), SecurityInitOut' (..))
-- import LocalCooking.Client.Dependencies.AccessToken.Generic (AuthInitIn (..), AuthInitOut (..))
import Facebook.State (FacebookLoginUnsavedFormData (FacebookLoginUnsavedFormDataSecurity))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.URI.Location (class ToLocation)
import Data.UUID (GENUUID)
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Text.Email.Validate as Email
import Control.Monad.Base (liftBase)
import Control.Monad.Eff.Ref (REF, Ref)
-- import Control.Monad.Eff.Ref.Extra (takeRef)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Queue.WhileMounted as Queue
import React.Signal.WhileMounted as Signal

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Button as Button
import MaterialUI.Divider (divider)
import Crypto.Scrypt (SCRYPT)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import IxSignal.Extra (onAvailable, getAvailable, getWhen)
import Queue.Types (readOnly, writeOnly, allowReading)
import Queue (WRITE, READ)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxQueue (IxQueue)
import IxQueue as IxQueue



-- TODO render social login


type State =
  { rerender :: Unit
  , user :: Maybe User
  }

initialState :: State
initialState =
  { rerender: unit
  , user: Nothing
  }

data Action
  = SubmitSecurity
  | GotUser User
  | ReceivedUnsavedFormData SecurityUnsavedFormData
  | ReRender


type Effects eff =
  ( ref :: REF
  , uuid :: GENUUID
  , exception :: EXCEPTION
  , scrypt :: SCRYPT
  | eff)


spec :: forall eff siteLinks userDetails
      . ToLocation siteLinks
     => LocalCookingParams siteLinks userDetails (Effects eff)
     -> { globalErrorQueue        :: One.Queue (write :: WRITE) (Effects eff) GlobalError
        , env                     :: Env
        , getUserQueues           :: GetUserSparrowClientQueues (Effects eff)
        , setUserQueues           :: SetUserSparrowClientQueues (Effects eff)
        , authenticateDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe HashedPassword)
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
        , pendingSignal            :: IxSignal (Effects eff) Boolean
        } -> T.Spec (Effects eff) State Unit Action
spec
  params
  { globalErrorQueue
  , env
  , getUserQueues
  , setUserQueues
  , authenticateDialogQueue
  , email
  , emailConfirm
  , password
  , passwordConfirm
  , submit
  , pendingSignal
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ReRender -> void $ T.cotransform _ {rerender = unit}
      GotUser u@(User xs) -> do
        void $ T.cotransform _ {user = Just u}
        liftEff $ do
          One.putQueue email.setQueue (Email.EmailGood xs.email)
          One.putQueue emailConfirm.setQueue (Email.EmailGood xs.email)
      ReceivedUnsavedFormData (SecurityUnsavedFormData xs@{socialLogin}) -> do
        liftEff $ do
          One.putQueue email.setQueue (Email.EmailPartial xs.email)
          One.putQueue emailConfirm.setQueue (Email.EmailPartial xs.emailConfirm)
        case state.user of
          Nothing -> pure unit
          Just (User u) ->
            void $ T.cotransform _ {user = Just $ User $ u {socialLogin = socialLogin}}
      SubmitSecurity -> do
        liftEff $ IxSignal.set true pendingSignal
        authToken <- liftBase $ getAvailable params.authTokenSignal
        let whenEmailGood mX = case mX of
              Email.EmailGood x -> Just x
              _ -> Nothing
        email <- liftBase $ getWhen whenEmailGood email.signal
        mAuthPass <- liftBase $ OneIO.callAsync authenticateDialogQueue unit
        case mAuthPass of
          Nothing -> pure unit
          Just oldPassword -> do
            mErr <- liftBase $ do
              passwordString <- liftEff (IxSignal.get password.signal)
              newPassword <- hashPassword
                { password: passwordString
                , salt: env.salt
                }
              -- FIXME assigning new password is a restricted credential set
              case state.user of
                Nothing -> pure Nothing
                Just (User {id,socialLogin}) ->
                  OneIO.callAsync setUserQueues $ AccessInitIn
                    { token: authToken
                    , subj: SetUser {id,email,socialLogin,oldPassword,newPassword}
                    }
            liftEff $ do
              One.putQueue globalErrorQueue $ case mErr of
                Nothing -> GlobalErrorSecurity SecuritySaveFailed
                Just JSONUnit -> GlobalErrorSecurity SecuritySaveSuccess
              IxSignal.set false pendingSignal

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: Typography.display1
        , align: Typography.center
        } [R.text "Security"]
      , R.div [RP.style {marginBotton: "1em"}] []
      , divider {}
      , Email.email
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
          case state.user of
            Nothing -> []
            Just (User {socialLogin}) ->
              mkSocialLogin params
                { env
                , getUnsavedFormData: do
                    email' <- IxSignal.get email.signal
                    emailConfirm' <- IxSignal.get emailConfirm.signal
                    pure $ FacebookLoginUnsavedFormDataSecurity
                      { email: case email' of
                          Email.EmailPartial e -> e
                          Email.EmailBad e -> e
                          Email.EmailGood e -> Email.toString e
                      , emailConfirm: case emailConfirm' of
                          Email.EmailPartial e -> e
                          Email.EmailBad e -> e
                          Email.EmailGood e -> Email.toString e
                      , socialLogin: socialLogin
                      }
                }
                socialLogin
      , Submit.submit
        { color: Button.secondary
        , variant: Button.raised
        , size: Button.large
        , style: createStyles {marginTop: "1em"}
        , disabledSignal: submit.disabledSignal
        , triggerQueue: submit.queue
        } [R.text "Submit"]
      , pending
        { pendingSignal
        }
      ]
      where
        passwordErrorQueue = unsafePerformEff $ writeOnly <$> One.newQueue
        passwordConfirmErrorQueue = unsafePerformEff $ writeOnly <$> One.newQueue



newtype SecurityUnsavedFormData = SecurityUnsavedFormData
  { email :: String
  , emailConfirm :: String
  , socialLogin :: SocialLoginForm
  }


security :: forall eff siteLinks userDetails
          . ToLocation siteLinks
         => LocalCookingParams siteLinks userDetails (Effects eff)
         -> { globalErrorQueue        :: One.Queue (write :: WRITE) (Effects eff) GlobalError
            , authenticateDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe HashedPassword)
            , getUserQueues           :: GetUserSparrowClientQueues (Effects eff)
            , setUserQueues           :: SetUserSparrowClientQueues (Effects eff)
            , env                     :: Env
            , unsavedFormDataQueue    :: One.Queue (write :: WRITE) (Effects eff) SecurityUnsavedFormData
            }
         -> R.ReactElement
security
  params
  { globalErrorQueue
  , authenticateDialogQueue
  , env
  , getUserQueues
  , setUserQueues
  , unsavedFormDataQueue
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            params
            { env
            , globalErrorQueue
            , authenticateDialogQueue
            , getUserQueues
            , setUserQueues
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
            , pendingSignal
            } ) initialState
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
                pure (mEmail /= confirm || p1 /= p2)
          _ -> pure true
        IxSignal.set x submitDisabledSignal
        unsafeCoerceEff $ dispatcher this ReRender
      reactSpec' =
          Queue.whileMountedIx
            submitQueue
            "onSubmit"
            (\this _ -> unsafeCoerceEff $ dispatcher this SubmitSecurity)
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
        $   reactSpec
              { componentDidMount = \this -> do
                  let getUserData authToken =
                        unsafeCoerceEff $ OneIO.callAsyncEff getUserQueues
                          (\mUser -> case mUser of
                            Nothing -> pure unit
                            Just u -> unsafeCoerceEff $ dispatcher this $ GotUser u
                          )
                          (AccessInitIn {token: authToken, subj: JSONUnit})
                  unsafeCoerceEff $ onAvailable
                    getUserData
                    params.authTokenSignal
              }
  in  R.createElement (R.createClass reactSpec') unit []
  where
    emailSignal = unsafePerformEff $ IxSignal.make $ Email.EmailPartial "" 
    emailConfirmSignal = unsafePerformEff $ IxSignal.make $ Email.EmailPartial ""
    emailUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    emailConfirmUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    emailSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    emailConfirmSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    passwordSignal = unsafePerformEff (IxSignal.make "")
    passwordUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    passwordConfirmSignal = unsafePerformEff (IxSignal.make "")
    passwordConfirmUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    pendingSignal = unsafePerformEff (IxSignal.make false)
    submitQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    submitDisabledSignal = unsafePerformEff (IxSignal.make true)
