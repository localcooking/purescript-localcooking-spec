module LocalCooking.Spec.Content.UserDetails.Security where

import LocalCooking.Spec.Common.Pending (pending)
import LocalCooking.Spec.Common.Form.Email (EmailState (..), email) as Email
import LocalCooking.Spec.Common.Form.Password as Password
import LocalCooking.Spec.Common.Form.Submit as Submit
import LocalCooking.Spec.Misc.Social (mkSocialLogin)
import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Global.Error (GlobalError)
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks)
import LocalCooking.Global.User.Class (class UserDetails, getUser)
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, initLocalCookingState, performActionLocalCooking, whileMountedLocalCooking)
import LocalCooking.Common.User.Password (HashedPassword, hashPassword)
import LocalCooking.Dependencies.Common (UserDeltaIn (UserDeltaInSetUser))
import LocalCooking.Semantics.Common (User (..), SetUser (..), SocialLoginForm, ChangePassword (..))
import Facebook.State (FacebookLoginUnsavedFormData (FacebookLoginUnsavedFormDataSecurity))

import Prelude
import Data.Maybe (Maybe (..))
import Data.URI.Location (class ToLocation)
import Data.UUID (GENUUID)
import Data.Lens (Lens', lens)
import Text.Email.Validate (toString) as Email
import Control.Monad.Base (liftBase)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (div, text) as R
import React.DOM.Props as RP
import React.Queue.WhileMounted as Queue

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Button as Button
import MaterialUI.Divider (divider)
import Crypto.Scrypt (SCRYPT)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import IxSignal.Extra (getAvailable, getWhen)
import Queue.Types (readOnly, writeOnly, allowReading)
import Queue (WRITE, READ)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxQueue (IxQueue)
import IxQueue as IxQueue



-- TODO render social login


type State siteLinks userDetails =
  { rerender :: Unit
  , user :: Maybe User
    -- represents the local mutable form state
  , localCooking :: LocalCookingState siteLinks userDetails
    -- represents the global saved actual state
  }

initialState :: forall siteLinks userDetails
              . UserDetails userDetails
             => LocalCookingState siteLinks userDetails -> State siteLinks userDetails
initialState localCooking =
  { rerender: unit
  , user: getUser <$> localCooking.userDetails
  , localCooking
  }

data Action siteLinks userDetails
  = SubmitSecurity
  | ReceivedUnsavedFormData SecurityUnsavedFormData
  | ReRender
  | LocalCookingAction (LocalCookingAction siteLinks userDetails)


type Effects eff =
  ( ref :: REF
  , uuid :: GENUUID
  , exception :: EXCEPTION
  , scrypt :: SCRYPT
  | eff)


getLCState :: forall siteLinks userDetails
            . Lens' (State siteLinks userDetails) (LocalCookingState siteLinks userDetails)
getLCState = lens (_.localCooking) (_ { localCooking = _ })



spec :: forall eff siteLinks userDetails userDetailsLinks
      . UserDetails userDetails
     => LocalCookingSiteLinks siteLinks userDetailsLinks
     => ToLocation siteLinks
     => LocalCookingParams siteLinks userDetails (Effects eff)
     -> { globalErrorQueue        :: One.Queue (write :: WRITE) (Effects eff) GlobalError
        , env                     :: Env
        , authenticateDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe HashedPassword)
        , userDeltaIn             :: UserDeltaIn -> Eff (Effects eff) Unit
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
        , pendingSignal    :: IxSignal (Effects eff) Boolean
        } -> T.Spec (Effects eff) (State siteLinks userDetails) Unit (Action siteLinks userDetails)
spec
  params
  { globalErrorQueue
  , env
  , authenticateDialogQueue
  , userDeltaIn
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
      LocalCookingAction a -> performActionLocalCooking getLCState a props state
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
        mEmail <- do
          mX <- liftEff $ IxSignal.get email.signal
          pure $ case mX of
            Email.EmailGood x -> Just x
            _ -> Nothing
        mChangePass <- do
          passwordString <- liftEff (IxSignal.get password.signal)
          if passwordString == ""
            then pure Nothing
            else do
              mAuthPass <- liftBase $ OneIO.callAsync authenticateDialogQueue unit
              case mAuthPass of
                Nothing -> pure Nothing
                Just oldPassword -> do
                  newPassword <- liftBase $ hashPassword
                    { password: passwordString
                    , salt: env.salt
                    }
                  pure $ Just $ ChangePassword {oldPassword, newPassword}
        -- FIXME assigning new password is a restricted credential set
        case state.user of
          Nothing -> pure unit
          Just (User {id,socialLogin}) ->
            liftEff $ userDeltaIn
                    $ UserDeltaInSetUser
                    $ SetUser
              { id
              , email: mEmail
              , socialLogin: Just socialLogin
              , changePassword: mChangePass
              }
        liftEff $ do
          -- One.putQueue globalErrorQueue $ case mErr of
          --   Nothing -> GlobalErrorSecurity SecuritySaveFailed
          --   Just JSONUnit -> GlobalErrorSecurity SecuritySaveSuccess
          -- FIXME threaded...?
          IxSignal.set false pendingSignal

    render :: T.Render (State siteLinks userDetails) Unit (Action siteLinks userDetails)
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
        , fullWidth: false
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



-- FIXME doesn't set or get security data?

security :: forall eff siteLinks userDetails userDetailsLinks
          . UserDetails userDetails
         => Eq siteLinks
         => Eq userDetails
         => LocalCookingSiteLinks siteLinks userDetailsLinks
         => ToLocation siteLinks
         => LocalCookingParams siteLinks userDetails (Effects eff)
         -> { globalErrorQueue        :: One.Queue (write :: WRITE) (Effects eff) GlobalError
            , authenticateDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe HashedPassword)
            , userDeltaIn             :: UserDeltaIn -> Eff (Effects eff) Unit
            , env                     :: Env
            , unsavedFormDataQueue    :: One.Queue (write :: WRITE) (Effects eff) SecurityUnsavedFormData
            }
         -> R.ReactElement
security
  params
  { globalErrorQueue
  , authenticateDialogQueue
  , env
  , userDeltaIn
  , unsavedFormDataQueue
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            params
            { env
            , globalErrorQueue
            , authenticateDialogQueue
            , userDeltaIn
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
            } ) (initialState (unsafePerformEff (initLocalCookingState params)))
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
        $ whileMountedLocalCooking
            params
            "LocalCooking.Spec.Content.UserDetails.Security"
            LocalCookingAction
            (\this -> unsafeCoerceEff <<< dispatcher this)
            reactSpec
              -- { componentDidMount = \this -> do
              --     let getUserData authToken =
              --           unsafeCoerceEff $ OneIO.callAsyncEff getUserQueues
              --             (\mUser -> case mUser of
              --               Nothing -> pure unit
              --               Just u -> unsafeCoerceEff $ dispatcher this $ GotUser u
              --             )
              --             (AccessInitIn {token: authToken, subj: JSONUnit})
              --     unsafeCoerceEff $ onAvailable
              --       getUserData
              --       params.authTokenSignal
              --     reactSpec.componentDidMount this
              -- }
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
