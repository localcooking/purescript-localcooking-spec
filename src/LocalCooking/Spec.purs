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
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Dependencies (DependenciesQueues)
import LocalCooking.Dependencies.Common (UserInitIn, UserDeltaIn)
import LocalCooking.Semantics.Common (Login)
import Auth.AccessToken.Session (SessionTokenInitIn, SessionTokenDeltaIn)

import Prelude
import Data.Password (HashedPassword)
import Data.URI.Location (Location, class ToLocation)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE)
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



type State = Unit


initialState :: State
initialState = unit


type Action = Unit


type Effects eff =
  ( ref        :: REF
  , exception  :: EXCEPTION
  , uuid       :: GENUUID
  , dom        :: DOM
  , now        :: NOW
  , timer      :: TIMER
  , webStorage :: WEB_STORAGE
  , scrypt     :: SCRYPT
  , console    :: CONSOLE
  | eff)



type TemplateArgs eff siteLinks userDetails =
  { content :: LocalCookingParams siteLinks userDetails eff -> R.ReactElement
  , topbar ::
    { imageSrc :: Location
    , buttons :: LocalCookingParams siteLinks userDetails eff -> Array R.ReactElement -> R.ReactElement
    }
  , leftDrawer ::
    { buttons :: LocalCookingParams siteLinks userDetails eff -> R.ReactElement -> R.ReactElement
    }
  , userDetails ::
    { buttons :: LocalCookingParams siteLinks userDetails eff -> Array R.ReactElement -> R.ReactElement -> R.ReactElement
    , content :: LocalCookingParams siteLinks userDetails eff -> R.ReactElement
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
    { content :: R.ReactElement
    }
  }




spec :: forall eff siteLinks userDetailsLinks userDetails
      . LocalCookingSiteLinks siteLinks userDetailsLinks
     => Eq siteLinks
     => Eq userDetails
     => ToLocation siteLinks
     => UserDetails userDetails
     => LocalCookingParams siteLinks userDetails (Effects eff)
     -> { env                 :: Env
        , globalErrorQueue    :: One.Queue (read :: READ, write :: WRITE) (Effects eff) GlobalError
        , dependenciesQueues  :: DependenciesQueues (Effects eff)
        , sessionTokenInitIn     :: SessionTokenInitIn Login -> Eff (Effects eff) Unit
        , sessionTokenDeltaIn    :: SessionTokenDeltaIn -> Eff (Effects eff) Unit
        , userInitIn          :: UserInitIn -> Eff (Effects eff) Unit
        , userDeltaIn         :: UserDeltaIn -> Eff (Effects eff) Unit
        -- FIXME ambiguate dependencies APIs
        , dialogQueues :: AllDialogs (Effects eff)
        , templateArgs :: TemplateArgs (Effects eff) siteLinks userDetails
        -- FIXME rename? How could these args be described as spec arguments?
        }
     -> T.Spec (Effects eff) State Unit Action
spec
  params
  { env
  , globalErrorQueue
  , dependenciesQueues
  , sessionTokenInitIn
  , sessionTokenDeltaIn
  , userInitIn
  , userDeltaIn
  , dialogQueues
  , templateArgs
  } = T.simpleSpec performAction render
  where
    performAction _ _ _ = pure unit

    render :: T.Render State Unit Action
    render dispatch props state children = template $
      [ topbar
        params
        { loginDialogQueue: dialogQueues.login.openQueue
        , sessionTokenInitIn
        , mobileMenuButtonTrigger: writeOnly mobileMenuButtonTrigger
        , imageSrc: templateArgs.topbar.imageSrc
        , buttons: templateArgs.topbar.buttons
        }
      , content
        params
        { env
        , globalErrorQueue
        , dependenciesQueues
        , sessionTokenInitIn
        , sessionTokenDeltaIn
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
      , templateArgs.error.content
      ]
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



app :: forall eff siteLinks userDetailsLinks userDetails
     . LocalCookingSiteLinks siteLinks userDetailsLinks
    => Eq siteLinks
    => Eq userDetails
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
       , dependenciesQueues :: DependenciesQueues (Effects eff)
       -- FIXME TODO restrict sessionTokenQueues from being visible
       , sessionTokenInitIn  :: SessionTokenInitIn Login -> Eff (Effects eff) Unit
       , sessionTokenDeltaIn :: SessionTokenDeltaIn -> Eff (Effects eff) Unit
       , userInitIn       :: UserInitIn -> Eff (Effects eff) Unit
       , userDeltaIn      :: UserDeltaIn -> Eff (Effects eff) Unit
       , templateArgs     :: TemplateArgs (Effects eff) siteLinks userDetails
       }
    -> { spec :: R.ReactSpec Unit State (Array R.ReactElement) (Effects eff)
       , dispatcher :: R.ReactThis Unit State -> Action -> T.EventHandler
       }
app
  params
  { env
  , globalErrorQueue
  , dialogQueues
  , dependenciesQueues
  , sessionTokenInitIn
  , sessionTokenDeltaIn
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
          , sessionTokenInitIn
          , sessionTokenDeltaIn
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
        ) initialState

  in  {spec: reactSpec, dispatcher}
  where
    -- FIXME dialog queues - would there be any spawned by a dep's async incoming?
    loginDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Login)
    loginDialogQueue = unsafePerformEff OneIO.newIOQueues

    authenticateDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe HashedPassword)
    authenticateDialogQueue = unsafePerformEff OneIO.newIOQueues

    privacyPolicyDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Unit)
    privacyPolicyDialogQueue = unsafePerformEff OneIO.newIOQueues
