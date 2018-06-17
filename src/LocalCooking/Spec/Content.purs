module LocalCooking.Spec.Content where

import LocalCooking.Spec.Content.UserDetails.Security (security, SecurityUnsavedFormData)
import LocalCooking.Spec.Content.Register (register, RegisterUnsavedFormData)
import LocalCooking.Spec.Misc.Flags.USA (usaFlag, usaFlagViewBox)
import LocalCooking.Spec.Misc.Flags.Colorado (coloradoFlag, coloradoFlagViewBox)
import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Spec.Dialogs (AllDialogs)
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingState, initLocalCookingState, performActionLocalCooking, LocalCookingAction, whileMountedLocalCooking)
import LocalCooking.Dependencies (DependenciesQueues)
import LocalCooking.Dependencies.AuthToken (AuthTokenInitIn, AuthTokenDeltaIn (AuthTokenDeltaInLogout))
import LocalCooking.Dependencies.Common (UserDeltaIn)
import LocalCooking.Global.Error (GlobalError)
import LocalCooking.Global.User.Class (class UserDetails)
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks, getUserDetailsLink, userDetailsLink, userDetailsGeneralLink, userDetailsSecurityLink, registerLink)

import Prelude
import Data.UUID (GENUUID)
import Data.URI.Location (class ToLocation)
import Data.Maybe (Maybe (..))
import Data.Lens (Lens', Prism', lens, prism')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Timer (TIMER)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (div, main, text) as R
import React.DOM.Props as RP
import React.DOM.SVG as RS
import MaterialUI.MuiThemeProvider (ColorPalette)
import MaterialUI.Paper (paper)
import MaterialUI.Divider (divider)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Drawer (drawer)
import MaterialUI.Drawer as Drawer
import MaterialUI.List (list)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemText (listItemText)
import MaterialUI.Types (createStyles)
import DOM.HTML.Window.Extra (WindowSize (Laptop))
import Browser.WebStorage (WEB_STORAGE)
import Crypto.Scrypt (SCRYPT)

import Queue.Types (writeOnly)
import Queue (READ, WRITE)
import Queue.One as One



type State siteLinks userDetails = LocalCookingState siteLinks userDetails


initialState :: forall siteLinks userDetails
              . LocalCookingState siteLinks userDetails -> State siteLinks userDetails
initialState = id


type Action siteLinks userDetails = LocalCookingAction siteLinks userDetails


type Effects eff =
  ( ref        :: REF
  , exception  :: EXCEPTION
  , uuid       :: GENUUID
  , timer      :: TIMER
  , webStorage :: WEB_STORAGE
  , scrypt     :: SCRYPT
  | eff)

getLCState :: forall siteLinks userDetails. Lens' (State siteLinks userDetails) (LocalCookingState siteLinks userDetails)
getLCState = lens id (\_ x -> x)

getLCAction :: forall siteLinks userDetails. Prism' (Action siteLinks userDetails) (LocalCookingAction siteLinks userDetails)
getLCAction = prism' id Just



spec :: forall eff siteLinks userDetailsLinks userDetails siteQueues
      . ToLocation siteLinks
     => LocalCookingSiteLinks siteLinks userDetailsLinks
     => Eq siteLinks
     => UserDetails userDetails
     => LocalCookingParams siteLinks userDetails (Effects eff)
     -> { env                :: Env
        , globalErrorQueue   :: One.Queue (read :: READ, write :: WRITE) (Effects eff) GlobalError
        , dependenciesQueues :: DependenciesQueues siteQueues (Effects eff)
        , authTokenInitIn    :: AuthTokenInitIn -> Eff (Effects eff) Unit
        , authTokenDeltaIn   :: AuthTokenDeltaIn -> Eff (Effects eff) Unit
        , userDeltaIn        :: UserDeltaIn -> Eff (Effects eff) Unit
        , dialogQueues :: AllDialogs (Effects eff)
        , templateArgs ::
          { content :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
          , security ::
            { unsavedFormDataQueue :: One.Queue (write :: WRITE) (Effects eff) SecurityUnsavedFormData
            }
          , register ::
            { unsavedFormDataQueue :: One.Queue (write :: WRITE) (Effects eff) RegisterUnsavedFormData
            }
          , userDetails ::
            { buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            , content :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            }
          , palette :: {primary :: ColorPalette, secondary :: ColorPalette}
          , extendedNetwork :: Array R.ReactElement
          }
        }
     -> T.Spec (Effects eff) (State siteLinks userDetails) Unit (Action siteLinks userDetails)
spec
  params
  { env
  , globalErrorQueue
  , dependenciesQueues
  , authTokenInitIn
  , authTokenDeltaIn
  , userDeltaIn
  , dialogQueues
  , templateArgs
  } = T.simpleSpec performAction render
  where
    performAction = performActionLocalCooking getLCState

    render :: T.Render (State siteLinks userDetails) Unit (Action siteLinks userDetails)
    render dispatch props state children =
      [ R.main [RP.style {marginTop: "4.5em"}]
        [ paper
          { style: if state.windowSize < Laptop
                      then createStyles
                              { width: "100%"
                              , position: "relative"
                              , minHeight: "30em"
                              , padding: "1em"
                              }
                      else createStyles
                              { maxWidth: "80em"
                              , width: "100%"
                              , marginLeft: "auto"
                              , marginRight: "auto"
                              , padding: "1em"
                              , position: "relative"
                              , minHeight: "30em"
                              , display: "flex"
                              , flexDirection: "row"
                              }
          } $ case getUserDetailsLink state.currentPage of
            Just mUserDetails ->
              -- TODO responsive design for side-drawer navigation
              -- FIXME User details component
              [ R.div []
                [ Drawer.withStyles
                  (\_ -> {paper: createStyles {position: "relative", width: "200px", zIndex: 1000}})
                  \{classes} -> drawer
                    { variant: Drawer.permanent
                    , anchor: Drawer.left
                    , classes: Drawer.createClasses classes
                    }
                    [ list {} $
                      [ listItem
                        { button: true
                        , onClick: mkEffFn1 \_ -> unsafeCoerceEff
                                                $ params.siteLinks $ userDetailsLink
                                                $ Just userDetailsGeneralLink
                        }
                        [ listItemText
                          { primary: "General"
                          }
                        ]
                      , divider {}
                      , listItem
                        { button: true
                        , onClick: mkEffFn1 \_ -> unsafeCoerceEff
                                                $ params.siteLinks $ userDetailsLink
                                                $ Just userDetailsSecurityLink
                        }
                        [ listItemText
                          { primary: "Security"
                          }
                        ]
                      , divider {}
                      ] <> templateArgs.userDetails.buttons params
                        <>
                      [ listItem
                        { button: true
                        , onClick: mkEffFn1 \_ -> unsafeCoerceEff
                                                $ authTokenDeltaIn AuthTokenDeltaInLogout -- pure unit -- dispatch Logout
                          -- FIXME feels weird - shouldn't this be its own sidebar component?
                        }
                        [ listItemText
                          { primary: "Logout"
                          }
                        ]
                      ]
                    ]
                  ]
              , R.div [RP.style {paddingLeft: "1em", width: "100%"}] $
                -- TODO pack currentPageSignal listener to this level, so side buttons
                -- aren't redrawn
                case mUserDetails of
                  Just d
                    | d == userDetailsSecurityLink ->
                      [ security
                        params
                        { env
                        , globalErrorQueue: writeOnly globalErrorQueue
                        , userDeltaIn
                        , authenticateDialogQueue: dialogQueues.authenticate.openQueue
                        , unsavedFormDataQueue: templateArgs.security.unsavedFormDataQueue
                        }
                      ]
                    | otherwise -> templateArgs.userDetails.content params
                  _ -> templateArgs.userDetails.content params
              ]

            _ | state.currentPage == registerLink ->
                  [ register
                    params
                    { registerQueues: dependenciesQueues.commonQueues.registerQueues
                    , globalErrorQueue: writeOnly globalErrorQueue
                    , privacyPolicyQueue: dialogQueues.privacyPolicy.openQueue
                    , unsavedFormDataQueue: templateArgs.register.unsavedFormDataQueue
                    , env
                    }
                  ]
              | otherwise ->
                  templateArgs.content params
        ]
      , -- FIXME footer component? Nah, just pack in content
        typography
        { variant: Typography.subheading
        , style: createStyles {color: "rgba(255,255,255,0.5)", marginTop: "5em"}
        , align: Typography.center
        }
        [ R.text "Extended Network"]
      , R.div
        [ RP.style {textAlign: "center", marginBottom: "5em"}
        ] templateArgs.extendedNetwork
      , divider {}
      , typography
        { variant: Typography.caption
        , style: createStyles {marginTop: "5em"}
        , align: Typography.center
        }
        [ R.text "Copyright © Local Cooking Inc. 2018, All rights reserved." ]
      , typography
        { variant: Typography.caption
        , align: Typography.center
        }
        [ R.text "Proudly made in Golden, Colorado, The United States of America."
        ]
      , R.div [RP.style {textAlign: "center", marginTop: "1em"}]
        [ RS.svg
          [ RP.viewBox coloradoFlagViewBox
          , RP.width (show flagWidth)
          , RP.height (show flagHeight)
          ] coloradoFlag
        , RS.svg
          [ RP.viewBox usaFlagViewBox
          , RP.width (show flagWidth)
          , RP.height (show flagHeight)
          ] usaFlag
        ]
      ]
      where
        flagWidth = 48
        flagHeight = 26


content :: forall eff siteLinks userDetailsLinks userDetails siteQueues
         . LocalCookingSiteLinks siteLinks userDetailsLinks
        => Eq siteLinks
        => ToLocation siteLinks
        => UserDetails userDetails
        => LocalCookingParams siteLinks userDetails (Effects eff)
        -> { env                :: Env
           , globalErrorQueue   :: One.Queue (read :: READ, write :: WRITE) (Effects eff) GlobalError
           -- FIXME TODO restrict authTokenQueues from being visible
           , dependenciesQueues :: DependenciesQueues siteQueues (Effects eff)
           , authTokenInitIn    :: AuthTokenInitIn -> Eff (Effects eff) Unit
           , authTokenDeltaIn   :: AuthTokenDeltaIn -> Eff (Effects eff) Unit
           , userDeltaIn        :: UserDeltaIn -> Eff (Effects eff) Unit
           , dialogQueues :: AllDialogs (Effects eff)
           , templateArgs ::
              { content :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
              , userDetails ::
                { buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
                , content :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
                }
              , palette :: {primary :: ColorPalette, secondary :: ColorPalette}
              , extendedNetwork :: Array R.ReactElement
              , security ::
                { unsavedFormDataQueue :: One.Queue (write :: WRITE) (Effects eff) SecurityUnsavedFormData
                }
              , register ::
                { unsavedFormDataQueue :: One.Queue (write :: WRITE) (Effects eff) RegisterUnsavedFormData
                }
              }
           }
        -> R.ReactElement
content
  params
  { env
  , globalErrorQueue
  , dependenciesQueues
  , authTokenInitIn
  , authTokenDeltaIn
  , userDeltaIn
  , dialogQueues
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
          , userDeltaIn
          , dialogQueues
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
  in  R.createElement (R.createClass reactSpec') unit []
