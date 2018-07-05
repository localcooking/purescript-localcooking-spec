module LocalCooking.Spec.Drawers.LeftMenu where

import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks, rootLink)
import LocalCooking.Thermite.Params
  (LocalCookingParams, LocalCookingStateLight, LocalCookingActionLight, initLocalCookingStateLight, performActionLocalCookingLight, whileMountedLocalCookingLight)

import Prelude
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Data.Lens (Lens', lens)
import Data.Time.Duration (Milliseconds (..))
import Data.DateTime.Instant (unInstant)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW, now)

import Thermite as T
import React as R
import React.Queue.WhileMounted as Queue

import MaterialUI.Drawer (drawer)
import MaterialUI.List (list)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemText (listItemText)
import MaterialUI.ListItemIcon (listItemIcon)
import MaterialUI.Icons.PersonPin (personPinIcon)

import Queue.One (READ, Queue)


type State siteLinks =
  { open :: Boolean
  , localCooking :: LocalCookingStateLight siteLinks
  }

initialState :: forall siteLinks. LocalCookingStateLight siteLinks -> State siteLinks
initialState localCooking =
  { open: false
  , localCooking
  }

data Action siteLinks
  = Clicked siteLinks
  | Open
  | Close
  | LocalCookingAction (LocalCookingActionLight siteLinks)


type Effects eff =
  ( ref       :: REF
  , exception :: EXCEPTION
  , uuid      :: GENUUID
  , now       :: NOW
  | eff)

getLCState :: forall siteLinks. Lens' (State siteLinks) (LocalCookingStateLight siteLinks)
getLCState = lens (_.localCooking) (_ { localCooking = _ })



spec :: forall eff siteLinks userDetailsLinks userDetails
      . LocalCookingSiteLinks siteLinks userDetailsLinks
     => LocalCookingParams siteLinks userDetails (Effects eff)
     -> { buttons :: LocalCookingParams siteLinks userDetails (Effects eff)
                  -> R.ReactElement -- About button
                  -> R.ReactElement
        }
     -> T.Spec (Effects eff) (State siteLinks) Unit (Action siteLinks)
spec
  params
  { buttons
  } = T.simpleSpec performAction render
  where
    lastOpen = unsafePerformEff (newRef Nothing)

    performAction action props state = case action of
      Clicked x -> do
        performAction Close props state
        liftEff (params.siteLinks x)
      Open -> do
        liftEff $ do
          n <- unInstant <$> now
          writeRef lastOpen (Just n)
        void $ T.cotransform _ { open = true }
      -- FIXME using now feels really shitty
      Close -> do
        mTuple <- liftEff $ do
          n <- unInstant <$> now
          mM <- readRef lastOpen
          case mM of
            Nothing -> pure Nothing
            Just m -> pure $ Just $ Tuple n m
        case mTuple of
          Nothing -> pure unit
          Just (Tuple n m)
            | n - m > Milliseconds 500.0 -> do
                liftEff (writeRef lastOpen Nothing)
                void $ T.cotransform _ { open = false }
            | otherwise -> pure unit
      LocalCookingAction a -> performActionLocalCookingLight getLCState a props state


    render :: T.Render (State siteLinks) Unit (Action siteLinks)
    render dispatch props state children =
      [ drawer
        { open: state.open
        , onClose: mkEffFn1 \_ -> dispatch Close
        }
        [ list {}
          [ buttons params $
            listItem
              { button: true
              , onClick: mkEffFn1 \_ -> dispatch $ Clicked $ rootLink :: siteLinks
              }
              [ listItemIcon {} personPinIcon
              , listItemText
                { primary: "About"
                }
              ]
          ]
        ]
      ]


leftMenu :: forall eff siteLinks userDetailsLinks userDetails
          . LocalCookingSiteLinks siteLinks userDetailsLinks
         => Eq siteLinks
         => LocalCookingParams siteLinks userDetails (Effects eff)
         -> { mobileDrawerOpenTrigger :: Queue (read :: READ) (Effects eff) Unit
            , buttons :: LocalCookingParams siteLinks userDetails (Effects eff)
                      -> R.ReactElement -- About button
                      -> R.ReactElement
            }
         -> R.ReactElement
leftMenu
  params
  { mobileDrawerOpenTrigger
  , buttons
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            params
            { buttons
            }
          )
          (initialState (unsafePerformEff (initLocalCookingStateLight params)))
      reactSpecLogin =
          whileMountedLocalCookingLight
            params
            "LocalCooking.Drawers.LeftMenu"
            LocalCookingAction
            (\this -> unsafeCoerceEff <<< dispatcher this)
        $ Queue.whileMountedOne
            mobileDrawerOpenTrigger
            (\this _ -> unsafeCoerceEff $ dispatcher this Open)
            reactSpec
  in  R.createElement (R.createClass reactSpecLogin) unit []
