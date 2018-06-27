module LocalCooking.Spec.Common.Form.TagSearch where

-- import LocalCooking.Common.Tag.Diet (DietTag (..))
import LocalCooking.Spec.Tag (tag, AnyTag)
import LocalCooking.Spec.Common.Form.Search as Search
import LocalCooking.Spec.Common.Form.SearchResults as SearchResults
import LocalCooking.Spec.Common.Form.Decisions as Decisions
import LocalCooking.Spec.Common.Form.Submit as Submit

import Prelude
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Data.Array as Array
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (text) as R
import React.Queue.WhileMounted as Queue
import React.Signal.WhileMounted as Signal

import MaterialUI.Types (createStyles)
import MaterialUI.Divider (divider)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid
import MaterialUI.Button as Button

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import IxQueue (IxQueue)
import IxQueue as IxQueue
import Queue.Types (writeOnly, readOnly, allowReading, READ, WRITE)
import Queue.One as One



type State = Unit

initialState :: State
initialState = unit

data Action tag
  = ClickedResult tag
  | DeleteDecision tag
  | SearchEntered
  | SearchUpdated String
  | GotResults (Array tag)


type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)


spec :: forall eff tag
      . Eq tag
     => { search ::
          { updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          , signal :: IxSignal (Effects eff) String
          , setQueue :: One.Queue (write :: WRITE) (Effects eff) String
          }
        , submit ::
          { triggerQueue :: IxQueue (read :: READ) (Effects eff) Unit
          , disabled :: IxSignal (Effects eff) Boolean
          }
        , results ::
          { setQueue :: One.Queue (write :: WRITE) (Effects eff) (Array tag)
          , addQueue :: One.Queue (write :: WRITE) (Effects eff) (Array tag)
          , clearQueue :: One.Queue (write :: WRITE) (Effects eff) Unit
          , pendingQueue :: One.Queue (write :: WRITE) (Effects eff) Unit
          , delQueue :: One.Queue (write :: WRITE) (Effects eff) tag
          , signal :: IxSignal (Effects eff) (SearchResults.SearchResults tag)
          }
        , decisions ::
          { addQueue :: One.Queue (write :: WRITE) (Effects eff) tag
          , delQueue :: One.Queue (write :: WRITE) (Effects eff) tag
          , signal :: IxSignal (Effects eff) (Array tag)
          }
        , tagSearch :: String -> Eff (Effects eff) Unit
        , label     :: String
        , headline  :: String
        , id        :: String
        , constructTag :: tag -> AnyTag
        }
     -> T.Spec (Effects eff) State Unit (Action tag)
spec
  { search
  , submit
  , results
  , decisions
  , tagSearch
  , label
  , headline
  , id
  , constructTag
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ClickedResult x -> liftEff $ do
        One.putQueue results.delQueue x
        One.putQueue decisions.addQueue x
      DeleteDecision x -> liftEff $ do
        One.putQueue decisions.delQueue x
        One.putQueue results.addQueue [x]
      SearchEntered -> liftEff $ do
        One.putQueue results.clearQueue unit
      SearchUpdated term -> liftEff $ do
        One.putQueue results.pendingQueue unit
        tagSearch term
      GotResults rs -> liftEff $ do
        One.putQueue results.setQueue rs
        IxSignal.set (not (Array.null rs)) submit.disabled

    render :: T.Render State Unit (Action tag)
    render dispatch props state children =
      [ typography
        { variant: Typography.headline
        } [R.text headline]
      , divider {}
      , grid {spacing: Grid.spacing8, container: true}
        [ grid {xs: 10, item: true}
          [ Search.search
            { label: R.text label
            , fullWidth: true
            , id
            , updatedQueue: search.updatedQueue
            , searchSignal: search.signal
            , setQueue: search.setQueue
            }
          ]
        , grid {xs: 2, item: true}
          [ Submit.submit
            { color: Button.primary
            , variant: Button.raised
            , size: Button.medium
            , style: createStyles {}
            , triggerQueue: submit.triggerQueue
            , disabledSignal: submit.disabled
            , fullWidth: true
            } [R.text "Add"]
          ]
        ]
      , SearchResults.results
        { setQueue: results.setQueue
        , addQueue: results.addQueue
        , clearQueue: results.clearQueue
        , pendingQueue: results.pendingQueue
        , delQueue: results.delQueue
        , resultsSignal: results.signal
        , renderA: \d ->
            tag
            { onClick: Just $ dispatch $ ClickedResult d
            , onDelete: Nothing
            , tag: constructTag d
            }
        }
      , Decisions.decisions
        { addQueue: decisions.addQueue
        , delQueue: decisions.delQueue
        , decisionsSignal: decisions.signal
        , renderA: \d ->
            tag
            { onClick: Nothing
            , onDelete: Just $ dispatch $ DeleteDecision d
            , tag: constructTag d
            }
        }
      ]


diets :: forall eff tag
       . Eq tag
      => { tagSearch    :: String -> Eff (Effects eff) Unit
         , resultsQueue :: One.Queue (write :: WRITE) (Effects eff) (Array tag)
         , label        :: String
         , headline     :: String
         , id           :: String
         , constructTag :: tag -> AnyTag
         }
      -> R.ReactElement
diets {tagSearch,label,headline,id,constructTag,resultsQueue} =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { search:
              { updatedQueue: searchUpdatedQueue
              , signal: searchSignal
              , setQueue: searchSetQueue
              }
            , submit:
              { triggerQueue: submitTriggerQueue
              , disabled: submitDisabled
              }
            , results:
              { setQueue: resultsSetQueue
              , addQueue: resultsAddQueue
              , clearQueue: resultsClearQueue
              , pendingQueue: resultsPendingQueue
              , delQueue: resultsDelQueue
              , signal: resultsSignal
              }
            , decisions:
              { addQueue: decisionsAddQueue
              , delQueue: decisionsDelQueue
              , signal: decisionsSignal
              }
            , tagSearch
            , label
            , headline
            , id
            , constructTag
            }
          )
          initialState
      reactSpec' =
          Queue.whileMountedIx
            searchUpdatedQueue
            "searchUpdated"
            (\this _ -> do
               term <- IxSignal.get searchSignal
               unsafeCoerceEff $ dispatcher this $ SearchUpdated term
            )
        $ Queue.whileMountedOne
            (allowReading resultsQueue)
            (\this rs -> unsafeCoerceEff $ dispatcher this $ GotResults rs)
        $ Signal.whileMountedIx
            searchSignal
            "searchEntered"
            (\this _ -> unsafeCoerceEff $ dispatcher this SearchEntered)
        $ reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
  where
    searchUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    searchSignal = unsafePerformEff $ IxSignal.make ""
    searchSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    resultsSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    resultsAddQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    resultsClearQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    resultsPendingQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    resultsDelQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    resultsSignal = unsafePerformEff $ IxSignal.make SearchResults.None
    decisionsAddQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    decisionsDelQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    decisionsSignal = unsafePerformEff $ IxSignal.make []
    submitTriggerQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    submitDisabled = unsafePerformEff $ IxSignal.make true
