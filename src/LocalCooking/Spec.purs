module LocalCooking.Spec where

import LocalCooking.Global.Error (GlobalError)
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks)
import LocalCooking.Global.User.Class (class UserDetails)
import LocalCooking.Spec.Topbar (topbar)
import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Spec.Types.Params
  ( LocalCookingParams, LocalCookingState, LocalCookingAction
  , performActionLocalCooking, whileMountedLocalCooking, initLocalCookingState)
import LocalCooking.Spec.Dialogs (dialogs)
import LocalCooking.Spec.Snackbar (messages)
import LocalCooking.Spec.Drawers.LeftMenu (leftMenu)
import LocalCooking.Dependencies (DependenciesQueues)
import LocalCooking.Dependencies.AuthToken (AuthTokenInitIn, AuthTokenDeltaIn)
import LocalCooking.Semantics.Common (Login)
import LocalCooking.Common.User.Password (HashedPassword)
-- import LocalCooking.Spec.Content (content)
-- import LocalCooking.Spec.Content.Register (register)
-- import LocalCooking.Spec.Content.UserDetails.Security (security)
-- import Facebook.State (FacebookLoginUnsavedFormData)

-- import Sparrow.Client.Queue (callSparrowClientQueues)

import Prelude
import Data.URI.Location (Location, class ToLocation)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
-- import Data.Either (Either (..))
import Data.Lens (Lens', Prism', lens, prism')
-- import Data.Generic (class Generic)
import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Console (CONSOLE, log)
-- import Control.Monad.Eff.Class (liftEff)
-- import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
-- import Control.Monad.Eff.Timer (TIMER, setTimeout)
-- import Control.Monad.Base (liftBase)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.DOM.SVG as RS
import MaterialUI.MuiThemeProvider (ColorPalette, muiThemeProvider, createMuiTheme)
import MaterialUI.CssBaseline (cssBaseline)
-- import MaterialUI.Paper (paper)
-- import MaterialUI.Divider (divider)
-- import MaterialUI.Typography (typography)
-- import MaterialUI.Typography as Typography
-- import MaterialUI.Drawer (drawer)
-- import MaterialUI.Drawer as Drawer
-- import MaterialUI.List (list)
-- import MaterialUI.ListItem (listItem)
-- import MaterialUI.ListItemText (listItemText)
-- import MaterialUI.Types (createStyles)
import DOM (DOM)
-- import DOM.HTML.Types (HISTORY)
-- import DOM.HTML.Window.Extra (WindowSize (Laptop))
-- import Browser.WebStorage (WEB_STORAGE)
import Crypto.Scrypt (SCRYPT)

import Queue.Types (writeOnly, readOnly)
import Queue (READ, WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxSignal.Internal as IxSignal



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
  -- , history    :: HISTORY
  , now        :: NOW
  -- , timer      :: TIMER
  -- , webStorage :: WEB_STORAGE
  -- , console    :: CONSOLE
  , scrypt     :: SCRYPT
  | eff)

getLCState :: forall siteLinks userDetails. Lens' (State siteLinks userDetails) (LocalCookingState siteLinks userDetails)
getLCState = lens id (\_ x -> x)

getLCAction :: forall siteLinks userDetails. Prism' (Action siteLinks userDetails) (LocalCookingAction siteLinks userDetails)
getLCAction = prism' id Just


spec :: forall eff siteLinks userDetailsLinks userDetails siteQueues
      . LocalCookingSiteLinks siteLinks userDetailsLinks
     => Eq siteLinks
     => ToLocation siteLinks
     => UserDetails userDetails
     -- => Generic siteLinks
     -- => Generic userDetails
     => LocalCookingParams siteLinks userDetails (Effects eff)
     -> { env                 :: Env
        , globalErrorQueue    :: One.Queue (read :: READ, write :: WRITE) (Effects eff) GlobalError
        , dependenciesQueues  :: DependenciesQueues siteQueues (Effects eff)
        , authTokenInitIn     :: AuthTokenInitIn -> Eff (Effects eff) Unit
        , authTokenDeltaIn    :: AuthTokenDeltaIn -> Eff (Effects eff) Unit
        -- FIXME ambiguate dependencies APIs
        , dialogQueues ::
          { login ::
            { openQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Login)
            , closeQueue :: One.Queue (write :: WRITE) (Effects eff) Unit
            }
          , authenticate ::
            { openQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe HashedPassword)
            }
          , privacyPolicy ::
            { openQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Unit)
            }
          }
        , templateArgs ::
          { content :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
          , topbar ::
            { imageSrc :: Location
            , buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            }
          , leftDrawer ::
            { buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            }
          , userDetails ::
            { buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            , content :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            }
          , palette :: {primary :: ColorPalette, secondary :: ColorPalette}
          , extendedNetwork :: Array R.ReactElement
          }
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
      ] -- <> content params
           -- { development
           -- , env
           -- , errorMessageQueue
           -- , dep
           -- }
        <> dialogs
           params
           { dialogQueues
           , dependencies:
             { passwordVerifyUnauthQueues:
                dependenciesQueues.validateQueues.passwordVerifyUnauthQueues
             , passwordVerifyQueues:
                dependenciesQueues.validateQueues.passwordVerifyQueues
             }
           , globalErrorQueue: writeOnly globalErrorQueue
           , env
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



app :: forall eff siteLinks userDetailsLinks userDetails siteQueues
     . LocalCookingSiteLinks siteLinks userDetailsLinks
    => Eq siteLinks
    => ToLocation siteLinks
    => UserDetails userDetails
    -- => Generic siteLinks
    -- => Generic userDetails
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
       , authTokenInitIn :: AuthTokenInitIn -> Eff (Effects eff) Unit
       , authTokenDeltaIn :: AuthTokenDeltaIn -> Eff (Effects eff) Unit
       , templateArgs ::
          { content :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
          , topbar ::
            { imageSrc :: Location
            , buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            }
          , leftDrawer ::
            { buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            }
          , userDetails ::
            { buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            , content :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            }
          , palette :: {primary :: ColorPalette, secondary :: ColorPalette}
          , extendedNetwork :: Array R.ReactElement
          }
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
