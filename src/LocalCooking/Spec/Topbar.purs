module LocalCooking.Spec.Topbar where

import LocalCooking.Global.Links.Class
  (class LocalCookingSiteLinks, rootLink, getUserDetailsLink, userDetailsLink)
import LocalCooking.Global.User.Class
  (class UserDetails, getUser)
import LocalCooking.Semantics.Common (Login, User (..))
import LocalCooking.Dependencies.AuthToken (AuthTokenInitIn (AuthTokenInitInLogin))
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, initLocalCookingState, performActionLocalCooking, whileMountedLocalCooking)

import Prelude
import Data.URI.URI (print) as URI
import Data.URI.Location (Location, class ToLocation, toLocation)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Data.Lens (Lens', lens)
import Text.Email.Validate as Email
import Control.Monad.Base (liftBase)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (div, img, text) as R
import React.DOM.Props as RP
import React.DOM.Props.PreventDefault (preventDefault)
import DOM.HTML.Window.Extra (WindowSize (..))

import MaterialUI.Types (createStyles)
import MaterialUI.Toolbar (toolbar)
import MaterialUI.AppBar (appBar)
import MaterialUI.AppBar as AppBar
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.IconButton (iconButton)
import MaterialUI.IconButton as IconButton
import MaterialUI.Icons.Menu (menuIcon)

import Queue.One (WRITE, Queue, putQueue)
import Queue.One.Aff as OneIO



type State siteLinks userDetails = LocalCookingState siteLinks userDetails

initialState :: forall siteLinks userDetails
              . LocalCookingState siteLinks userDetails -> State siteLinks userDetails
initialState = id

data Action siteLinks userDetails
  = AttemptLogin
  | ClickedMobileMenuButton
  | Clicked siteLinks
  | LocalCookingAction (LocalCookingAction siteLinks userDetails)

type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  | eff)

getLCState :: forall siteLinks userDetails. Lens' (State siteLinks userDetails) (LocalCookingState siteLinks userDetails)
getLCState = lens id (\_ x -> x)


spec :: forall eff siteLinks userDetailsLinks userDetails
      . LocalCookingSiteLinks siteLinks userDetailsLinks
     => Eq siteLinks
     => ToLocation siteLinks
     => UserDetails userDetails
     => LocalCookingParams siteLinks userDetails (Effects eff)
     -> { loginDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Login)
        , authTokenInitIn :: AuthTokenInitIn -> Eff (Effects eff) Unit
        , mobileMenuButtonTrigger :: Queue (write :: WRITE) (Effects eff) Unit
        , imageSrc :: Location
        , buttons :: LocalCookingParams siteLinks userDetails (Effects eff)
                  -> Array R.ReactElement -- Prefix
                  -> R.ReactElement
        }
     -> T.Spec (Effects eff) (State siteLinks userDetails) Unit (Action siteLinks userDetails)
spec
  params
  { loginDialogQueue
  , authTokenInitIn
  , mobileMenuButtonTrigger
  , imageSrc
  , buttons
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      AttemptLogin -> do
        mLogin <- liftBase (OneIO.callAsync loginDialogQueue unit)
        case mLogin of
          Nothing -> pure unit
          Just login -> liftEff $ authTokenInitIn $ AuthTokenInitInLogin login
      ClickedMobileMenuButton -> liftEff (putQueue mobileMenuButtonTrigger unit)
      Clicked x -> liftEff (params.siteLinks x)
      LocalCookingAction a -> performActionLocalCooking getLCState a props state

    render :: T.Render (State siteLinks userDetails) Unit (Action siteLinks userDetails)
    render dispatch props state children =
      [ appBar {color: AppBar.default, position: AppBar.fixed}
        [ toolbar {style: createStyles {display: "flex"}} $
          [ if state.windowSize < Laptop
            then
              iconButton
                { color: IconButton.inherit
                , onTouchTap: mkEffFn1 \_ -> dispatch ClickedMobileMenuButton
                } menuIcon
            else
              buttons params $
                [ R.img
                  [ RP.src $ URI.print $ params.toURI imageSrc
                  , RP.style {height: "2.5em", border: 0}
                  ] []
                , button
                  { color: Button.inherit
                  , disabled: state.currentPage == rootLink
                  , onClick: mkEffFn1 preventDefault
                  , onTouchTap: mkEffFn1 \e -> do
                      preventDefault e
                      dispatch $ Clicked $ rootLink :: siteLinks
                  , href: URI.print $ params.toURI $ toLocation $ rootLink :: siteLinks
                  , variant: Button.flat
                  } [R.text "About"]
                ]
          , R.div [RP.style {flex: 1, display: "flex", flexDirection: "row-reverse"}] $ case state.userDetails of
               Nothing ->
                [ button
                  { color: Button.inherit
                  , onTouchTap: mkEffFn1 \_ -> dispatch AttemptLogin
                  } [R.text "Login"]
                ]
               Just userDetails ->
                [ button -- TODO cart iconButton
                  { color: Button.inherit
                  , onTouchTap: mkEffFn1 \_ -> dispatch $ Clicked $ userDetailsLink Nothing :: siteLinks
                  , disabled: case getUserDetailsLink state.currentPage of
                    Just _ -> true
                    _ -> false
                  } [ case getUser userDetails of
                        User {email} -> R.text (Email.toString email)
                    ]
                ]
          ]
        ]
      ]



topbar :: forall eff siteLinks userDetailsLinks userDetails
        . LocalCookingSiteLinks siteLinks userDetailsLinks
       => Eq siteLinks
       => Eq userDetails
       => ToLocation siteLinks
       => UserDetails userDetails
       => LocalCookingParams siteLinks userDetails (Effects eff)
       -> { loginDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Login)
          , authTokenInitIn :: AuthTokenInitIn -> Eff (Effects eff) Unit
          , mobileMenuButtonTrigger :: Queue (write :: WRITE) (Effects eff) Unit
          , imageSrc :: Location
          , buttons :: LocalCookingParams siteLinks userDetails (Effects eff)
                    -> Array R.ReactElement
                    -> R.ReactElement
          } -> R.ReactElement
topbar
  params
  { loginDialogQueue
  , authTokenInitIn
  , mobileMenuButtonTrigger
  , imageSrc
  , buttons
  } =
  let {spec:reactSpec,dispatcher} = T.createReactSpec
        ( spec
          params
          { loginDialogQueue
          , authTokenInitIn
          , mobileMenuButtonTrigger
          , imageSrc
          , buttons
          }
        )
        (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
          whileMountedLocalCooking
            params
            "LocalCooking.Spec.Topbar"
            LocalCookingAction
            (\this -> unsafeCoerceEff <<< dispatcher this)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
