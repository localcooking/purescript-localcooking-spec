module LocalCooking.Spec.Common.Form.SearchResults where

import Prelude
import Data.Array as Array
import Control.Monad.Eff.Ref (REF)
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
  { results :: Array a
  , rerender :: Unit
  }

initialState :: forall a. {initResults :: Array a} -> State a
initialState {initResults} =
  { results: initResults
  , rerender: unit
  }

data Action a
  = SetResults (Array a)
  | AddResults (Array a)
  | DelResult a
  | ReRender

type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff a
      . Eq a
     => { resultsSignal :: IxSignal (Effects eff) (Array a)
        , renderA  :: a -> R.ReactElement
        } -> T.Spec (Effects eff) (State a) Unit (Action a)
spec
  { resultsSignal
  , renderA
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      SetResults xs -> do
        liftEff (IxSignal.set xs resultsSignal)
        void $ T.cotransform _ { results = xs }
        performAction ReRender props state
      AddResults xs -> do
        ys <- liftEff $ IxSignal.get resultsSignal
        let zs = ys <> xs
        liftEff $ IxSignal.set zs resultsSignal
        void $ T.cotransform _ { results = zs }
        performAction ReRender props state
      DelResult x -> do
        ys <- liftEff $ IxSignal.get resultsSignal
        let zs = Array.filter (\y -> y /= x) ys
        liftEff $ IxSignal.set zs resultsSignal
        void $ T.cotransform _ { results = zs }
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
        ] $ map renderA state.results
      ]



results :: forall eff a
         . Eq a
        => { setQueue :: One.Queue (write :: WRITE) (Effects eff) (Array a)
           , addQueue :: One.Queue (write :: WRITE) (Effects eff) (Array a)
           , delQueue :: One.Queue (write :: WRITE) (Effects eff) a
           , resultsSignal :: IxSignal (Effects eff) (Array a)
           , renderA :: a -> R.ReactElement
           } -> R.ReactElement
results {setQueue,addQueue,delQueue,resultsSignal,renderA} =
  let init =
        { initResults: unsafePerformEff (IxSignal.get resultsSignal)
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { resultsSignal
            , renderA
            } ) (initialState init)
      reactSpec' =
          Queue.whileMountedOne
            (allowReading setQueue)
            (\this x -> unsafeCoerceEff $ dispatcher this $ SetResults x)
        $ Queue.whileMountedOne
            (allowReading addQueue)
            (\this x -> unsafeCoerceEff $ dispatcher this $ AddResults x)
        $ Queue.whileMountedOne
            (allowReading delQueue)
            (\this x -> unsafeCoerceEff $ dispatcher this $ DelResult x)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
