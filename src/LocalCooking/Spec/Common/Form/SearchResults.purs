module LocalCooking.Spec.Common.Form.SearchResults where

import LocalCooking.Spec.Common.Pending (pending)

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (div) as R
import React.DOM.Props as RP
import React.Queue.WhileMounted as Queue

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue.Types (WRITE, allowReading)
import Queue.One as One



type State a =
  { results :: Maybe (Array a)
  , rerender :: Unit
  }

initialState :: forall a. {initResults :: Maybe (Array a)} -> State a
initialState {initResults} =
  { results: initResults
  , rerender: unit
  }

data Action a
  = SetResults (Array a)
  | AddResults (Array a)
  | ClearResults
  | DelResult a
  | ReRender

type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)


spec :: forall eff a
      . Eq a
     => { resultsSignal :: IxSignal (Effects eff) (Maybe (Array a))
        , renderA       :: a -> R.ReactElement
        , pendingSignal :: IxSignal (Effects eff) Boolean
        } -> T.Spec (Effects eff) (State a) Unit (Action a)
spec
  { resultsSignal
  , renderA
  , pendingSignal
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      SetResults xs -> do
        liftEff (IxSignal.set false pendingSignal)
        liftEff (IxSignal.set (Just xs) resultsSignal)
        void $ T.cotransform _ { results = Just xs }
        performAction ReRender props state
      AddResults xs -> do
        mYs <- liftEff $ IxSignal.get resultsSignal
        let zs = case mYs of
                    Nothing -> xs
                    Just ys -> ys <> xs
        liftEff $ IxSignal.set (Just zs) resultsSignal
        void $ T.cotransform _ { results = Just zs }
        performAction ReRender props state
      ClearResults -> do
        liftEff (IxSignal.set true pendingSignal)
        liftEff (IxSignal.set Nothing resultsSignal)
        void $ T.cotransform _ { results = Nothing }
        performAction ReRender props state
      DelResult x -> do
        mYs <- liftEff $ IxSignal.get resultsSignal
        let mZs = case mYs of
                    Nothing -> Nothing
                    Just ys -> Just $ Array.filter (\y -> y /= x) ys
        liftEff $ IxSignal.set mZs resultsSignal
        void $ T.cotransform _ { results = mZs }
        performAction ReRender props state
      ReRender -> void $ T.cotransform _ { rerender = unit }

    render :: T.Render (State a) Unit (Action a)
    render dispatch props state children =
      [ R.div
        [ RP.style
          { height: "10em"
          , overflowY: "auto"
          , padding: "0.5em"
          }
        ] $ case state.results of
          Nothing -> [pending {pendingSignal}]
          Just rs -> map renderA rs
      ]



results :: forall eff a
         . Eq a
        => { setQueue :: One.Queue (write :: WRITE) (Effects eff) (Array a)
           , addQueue :: One.Queue (write :: WRITE) (Effects eff) (Array a)
           , clearQueue :: One.Queue (write :: WRITE) (Effects eff) Unit
           , delQueue :: One.Queue (write :: WRITE) (Effects eff) a
           , resultsSignal :: IxSignal (Effects eff) (Maybe (Array a))
           , renderA :: a -> R.ReactElement
           } -> R.ReactElement
results {setQueue,addQueue,clearQueue,delQueue,resultsSignal,renderA} =
  let init =
        { initResults: unsafePerformEff (IxSignal.get resultsSignal)
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { resultsSignal
            , renderA
            , pendingSignal
            } ) (initialState init)
      reactSpec' =
          Queue.whileMountedOne
            (allowReading setQueue)
            (\this x -> unsafeCoerceEff $ dispatcher this $ SetResults x)
        $ Queue.whileMountedOne
            (allowReading addQueue)
            (\this x -> unsafeCoerceEff $ dispatcher this $ AddResults x)
        $ Queue.whileMountedOne
            (allowReading clearQueue)
            (\this _ -> unsafeCoerceEff $ dispatcher this ClearResults)
        $ Queue.whileMountedOne
            (allowReading delQueue)
            (\this x -> unsafeCoerceEff $ dispatcher this $ DelResult x)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
  where
    pendingSignal = unsafePerformEff $ IxSignal.make false
