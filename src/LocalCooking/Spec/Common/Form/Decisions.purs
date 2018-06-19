module LocalCooking.Spec.Common.Form.Decisions where

import Prelude
import Data.Array as Array
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.Queue.WhileMounted as Queue

import MaterialUI.Types (createStyles)
import MaterialUI.TextField (textField)
import MaterialUI.Input as Input
import MaterialUI.Input (inputAdornment)
import MaterialUI.Paper (paper)
import MaterialUI.Icons.Search (searchIcon)

import Unsafe.Coerce (unsafeCoerce)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue.Types (READ, WRITE, allowWriting, allowReading)
import Queue.One as One
import IxQueue (IxQueue)
import IxQueue as IxQueue



type State a =
  { decisions :: Array a
  , rerender  :: Unit
  }

initialState :: forall a. {initDecisions :: Array a} -> State a
initialState {initDecisions} =
  { decisions: initDecisions
  , rerender: unit
  }

data Action a
  = AddDecision a
  | DelDecision a
  | ReRender

type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff a
      . Eq a
     => { decisionsSignal :: IxSignal (Effects eff) (Array a)
        , renderA :: a -> R.ReactElement
        } -> T.Spec (Effects eff) (State a) Unit (Action a)
spec
  { decisionsSignal
  , renderA
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      AddDecision x -> do
        xs <- liftEff (IxSignal.get decisionsSignal)
        let ys = xs <> [x]
        liftEff (IxSignal.set ys decisionsSignal)
        void $ T.cotransform _ { decisions = ys }
      DelDecision x -> do
        xs <- liftEff (IxSignal.get decisionsSignal)
        let ys = Array.filter (\y -> y /= x) xs
        liftEff (IxSignal.set ys decisionsSignal)
        void $ T.cotransform _ { decisions = ys }
      ReRender -> void $ T.cotransform _ { rerender = unit }

    render :: T.Render (State a) Unit (Action a)
    render dispatch props state children =
      [ paper
        { style: createStyles
          { width: "100%"
          , padding: "0.5em"
          , marginBottom: "1em"
          , display: "flex"
          }
        } (map renderA state.decisions)
      ]



decisions :: forall eff a
           . Eq a
          => { addQueue       :: One.Queue (write :: WRITE) (Effects eff) a
             , delQueue       :: One.Queue (write :: WRITE) (Effects eff) a
             , decisionsSignal :: IxSignal (Effects eff) (Array a)
             , renderA :: a -> R.ReactElement
             } -> R.ReactElement
decisions {addQueue,delQueue,decisionsSignal,renderA} =
  let init =
        { initDecisions: unsafePerformEff (IxSignal.get decisionsSignal)
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { decisionsSignal
            , renderA
            } ) (initialState init)
      reactSpec' =
          Queue.whileMountedOne
            (allowReading addQueue)
            (\this x -> unsafeCoerceEff $ dispatcher this $ AddDecision x)
        $ Queue.whileMountedOne
            (allowReading delQueue)
            (\this x -> unsafeCoerceEff $ dispatcher this $ DelDecision x)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
