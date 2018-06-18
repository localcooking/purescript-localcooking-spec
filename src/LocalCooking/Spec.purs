module LocalCooking.Spec where

import LocalCooking.Global.Error (GlobalError)
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks)
import LocalCooking.Global.User.Class (class UserDetails)
import LocalCooking.Spec.Content.UserDetails.Security (SecurityUnsavedFormData)
import LocalCooking.Spec.Content.Register (RegisterUnsavedFormData)
import LocalCooking.Spec.Content (content)
import LocalCooking.Spec.Topbar (topbar)
import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Spec.Dialogs (AllDialogs, dialogs)
import LocalCooking.Spec.Snackbar (messages)
import LocalCooking.Spec.Drawers.LeftMenu (leftMenu)
import LocalCooking.Thermite.Params
  ( LocalCookingParams, LocalCookingState, LocalCookingAction
  , performActionLocalCooking, whileMountedLocalCooking, initLocalCookingState)
import LocalCooking.Dependencies (DependenciesQueues)
import LocalCooking.Dependencies.AuthToken (AuthTokenInitIn, AuthTokenDeltaIn)
import LocalCooking.Dependencies.Common (UserInitIn, UserDeltaIn)
import LocalCooking.Semantics.Common (Login)
import LocalCooking.Common.User.Password (HashedPassword)

import Prelude
import Data.URI.Location (Location, class ToLocation)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Data.Lens (Lens', Prism', lens, prism')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER)

import Thermite as T
import React (ReactElement, ReactThis, ReactSpec) as R
import React.DOM (div) as R
import MaterialUI.MuiThemeProvider (ColorPalette, muiThemeProvider, createMuiTheme)
import MaterialUI.CssBaseline (cssBaseline)
import DOM (DOM)
import Browser.WebStorage (WEB_STORAGE)
import Crypto.Scrypt (SCRYPT)

import Queue.Types (writeOnly, readOnly)
import Queue (READ, WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO



type State siteLinks userDetails = LocalCookingState siteLinks userDetails


initialState :: forall siteLinks userDetails
              . LocalCookingState siteLinks userDetails -> State siteLinks userDetails
initialState = id


type Action siteLinks userDetails = LocalCookingAction siteLinks userDetails


type Effects eff =
  ( ref        :: REF
  , exception  :: EXCEPTION
  , uuid       :: GENUUID
  , dom        :: DOM
  , now        :: NOW
  , timer      :: TIMER
  , webStorage :: WEB_STORAGE
  , scrypt     :: SCRYPT
  | eff)

getLCState :: forall siteLinks userDetails. Lens' (State siteLinks userDetails) (LocalCookingState siteLinks userDetails)
getLCState = lens id (\_ x -> x)

getLCAction :: forall siteLinks userDetails. Prism' (Action siteLinks userDetails) (LocalCookingAction siteLinks userDetails)
getLCAction = prism' id Just



type TemplateArgs eff siteLinks userDetails =
  { content :: LocalCookingParams siteLinks userDetails eff -> Array R.ReactElement
  , topbar ::
    { imageSrc :: Location
    , buttons :: LocalCookingParams siteLinks userDetails eff -> Array R.ReactElement
    }
  , leftDrawer ::
    { buttons :: LocalCookingParams siteLinks userDetails eff -> Array R.ReactElement
    }
  , userDetails ::
    { buttons :: LocalCookingParams siteLinks userDetails eff -> Array R.ReactElement
    , content :: LocalCookingParams siteLinks userDetails eff -> Array R.ReactElement
    }
  , palette :: {primary :: ColorPalette, secondary :: ColorPalette}
  , extendedNetwork :: Array R.ReactElement
  , security ::
    { unsavedFormDataQueue :: One.Queue (write :: WRITE) eff SecurityUnsavedFormData
    }
  , register ::
    { unsavedFormDataQueue :: One.Queue (write :: WRITE) eff RegisterUnsavedFormData
    }
  , error ::
    { content :: Array R.ReactElement
    }
  }




spec :: forall eff siteLinks userDetailsLinks userDetails siteQueues
      . LocalCookingSiteLinks siteLinks userDetailsLinks
     => Eq siteLinks
     => ToLocation siteLinks
     => UserDetails userDetails
     => LocalCookingParams siteLinks userDetails (Effects eff)
     -> { env                 :: Env
        , globalErrorQueue    :: One.Queue (read :: READ, write :: WRITE) (Effects eff) GlobalError
        , dependenciesQueues  :: DependenciesQueues siteQueues (Effects eff)
        , authTokenInitIn     :: AuthTokenInitIn -> Eff (Effects eff) Unit
        , authTokenDeltaIn    :: AuthTokenDeltaIn -> Eff (Effects eff) Unit
        , userInitIn          :: UserInitIn -> Eff (Effects eff) Unit
        , userDeltaIn         :: UserDeltaIn -> Eff (Effects eff) Unit
        -- FIXME ambiguate dependencies APIs
        , dialogQueues :: AllDialogs (Effects eff)
        , templateArgs :: TemplateArgs (Effects eff) siteLinks userDetails
        -- FIXME rename? How could these args be described as spec arguments?
        }
     -> T.Spec (Effects eff) (State siteLinks userDetails) Unit (Action siteLinks userDetails)
spec
  params
  { env
  , globalErrorQueue
  , dependenciesQueues
  , authTokenInitIn
  , authTokenDeltaIn
  , userInitIn
  , userDeltaIn
  , dialogQueues
  , templateArgs
  } = T.simpleSpec performAction render
  where
    performAction = performActionLocalCooking getLCState

    render :: T.Render (State siteLinks userDetails) Unit (Action siteLinks userDetails)
    render dispatch props state children = template $
      [ topbar
        params
        { loginDialogQueue: dialogQueues.login.openQueue
        , authTokenInitIn
        , mobileMenuButtonTrigger: writeOnly mobileMenuButtonTrigger
        , imageSrc: templateArgs.topbar.imageSrc
        , buttons: templateArgs.topbar.buttons
        }
      , content
        params
        { env
        , globalErrorQueue
        , dependenciesQueues
        , authTokenInitIn
        , authTokenDeltaIn
        , userDeltaIn
        , dialogQueues
        , templateArgs:
          { content: templateArgs.content
          , userDetails: templateArgs.userDetails
          , palette: templateArgs.palette
          , extendedNetwork: templateArgs.extendedNetwork
          , security: templateArgs.security
          , register: templateArgs.register
          }
        }
      ] <> dialogs
           params
           { env
           , dialogQueues
           , dependencies:
             { passwordVerifyUnauthQueues:
                dependenciesQueues.validateQueues.passwordVerifyUnauthQueues
             , passwordVerifyQueues:
                dependenciesQueues.validateQueues.passwordVerifyQueues
             }
           , globalErrorQueue: writeOnly globalErrorQueue
           }
        <>
      [ leftMenu
        params
        { mobileDrawerOpenTrigger: readOnly mobileMenuButtonTrigger
        , buttons: templateArgs.leftDrawer.buttons
        }
      , messages
        { globalErrorQueue: readOnly globalErrorQueue
        }
      ] <> templateArgs.error.content
      where
        template xs =
          [ cssBaseline
          , muiThemeProvider
              { theme: createMuiTheme
                { palette: templateArgs.palette
                }
              }
              (R.div [] xs)
          ]

        mobileMenuButtonTrigger :: One.Queue (read :: READ, write :: WRITE) (Effects eff) Unit
        mobileMenuButtonTrigger = unsafePerformEff One.newQueue



app :: forall eff siteLinks userDetailsLinks userDetails siteQueues
     . LocalCookingSiteLinks siteLinks userDetailsLinks
    => Eq siteLinks
    => ToLocation siteLinks
    => UserDetails userDetails
    => LocalCookingParams siteLinks userDetails (Effects eff)
    -> { env                  :: Env
       , globalErrorQueue     :: One.Queue (read :: READ, write :: WRITE) (Effects eff) GlobalError
       -- template links them
       , dialogQueues ::
         { login ::
           { closeQueue :: One.Queue (write :: WRITE) (Effects eff) Unit
           }
         }
       , dependenciesQueues :: DependenciesQueues siteQueues (Effects eff)
       -- FIXME TODO restrict authTokenQueues from being visible
       , authTokenInitIn  :: AuthTokenInitIn -> Eff (Effects eff) Unit
       , authTokenDeltaIn :: AuthTokenDeltaIn -> Eff (Effects eff) Unit
       , userInitIn       :: UserInitIn -> Eff (Effects eff) Unit
       , userDeltaIn      :: UserDeltaIn -> Eff (Effects eff) Unit
       , templateArgs     :: TemplateArgs (Effects eff) siteLinks userDetails
       }
    -> { spec :: R.ReactSpec Unit (State siteLinks userDetails) (Array R.ReactElement) (Effects eff)
       , dispatcher :: R.ReactThis Unit (State siteLinks userDetails) -> (Action siteLinks userDetails) -> T.EventHandler
       }
app
  params
  { env
  , globalErrorQueue
  , dialogQueues
  , dependenciesQueues
  , authTokenInitIn
  , authTokenDeltaIn
  , userInitIn
  , userDeltaIn
  , templateArgs
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
        ( spec
          params
          { env
          , globalErrorQueue
          , dependenciesQueues
          , authTokenInitIn
          , authTokenDeltaIn
          , userInitIn
          , userDeltaIn
          , dialogQueues:
            { login:
              { openQueue: loginDialogQueue
              , closeQueue: dialogQueues.login.closeQueue
              }
            , authenticate:
              { openQueue: authenticateDialogQueue
              }
            , privacyPolicy:
              { openQueue: privacyPolicyDialogQueue
              }
            }
          , templateArgs
          }
        ) (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
          whileMountedLocalCooking
            params
            "LocalCooking.Spec"
            id
            (\this -> unsafeCoerceEff <<< dispatcher this)
            reactSpec

  in  {spec: reactSpec', dispatcher}
  where
    -- FIXME dialog queues - would there be any spawned by a dep's async incoming?
    loginDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Login)
    loginDialogQueue = unsafePerformEff OneIO.newIOQueues

    authenticateDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe HashedPassword)
    authenticateDialogQueue = unsafePerformEff OneIO.newIOQueues

    privacyPolicyDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Unit)
    privacyPolicyDialogQueue = unsafePerformEff OneIO.newIOQueues
