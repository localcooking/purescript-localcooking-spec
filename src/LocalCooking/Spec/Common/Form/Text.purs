module LocalCooking.Spec.Common.Form.Text where

import Prelude
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.Queue.WhileMounted as Queue

import MaterialUI.TextField (textField)
import MaterialUI.Input as Input

import Unsafe.Coerce (unsafeCoerce)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue.Types (READ, WRITE, allowWriting, allowReading)
import Queue.One as One
import IxQueue (IxQueue)
import IxQueue as IxQueue



type State =
  { text :: String
  , rerender :: Unit
  }

initialState :: {initText :: String} -> State
initialState {initText} =
  { text: initText
  , rerender: unit
  }

data Action
  = ChangedText String
  | SetText String
  | TextUnfocused
  | ReRender

type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff
      . { textSignal   :: IxSignal (Effects eff) String
        , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
        , label        :: R.ReactElement
        , fullWidth    :: Boolean
        , id           :: String
        } -> T.Spec (Effects eff) State Unit Action
spec
  { textSignal
  , updatedQueue
  , label
  , fullWidth
  , id
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedText n -> do
        liftEff (IxSignal.set n textSignal)
        void $ T.cotransform _ { text = n }
      SetText x -> do
        liftEff $ unsafeCoerceEff $ log $ "setting: " <> x
        liftEff (IxSignal.set x textSignal)
        void $ T.cotransform _ { text = x }
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit
      TextUnfocused -> do
        liftEff $ IxSignal.set state.text textSignal
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit
      ReRender -> void $ T.cotransform _ { rerender = unit }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ textField
        { label
        , fullWidth
        , value: Input.valueString state.text
        , onChange: mkEffFn1 \e -> dispatch $ ChangedText (unsafeCoerce e).target.value
        , onBlur: mkEffFn1 \_ -> dispatch TextUnfocused
        , name: id
        , id
        } []
      ]



text :: forall eff
      . { label        :: R.ReactElement
        , fullWidth    :: Boolean
        , id           :: String
        , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
        , textSignal   :: IxSignal (Effects eff) String
        , setQueue     :: One.Queue (write :: WRITE) (Effects eff) String
        } -> R.ReactElement
text {label,fullWidth,id,updatedQueue,textSignal,setQueue} =
  let init =
        { initText: unsafePerformEff (IxSignal.get textSignal)
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { label
            , fullWidth
            , id
            , updatedQueue
            , textSignal
            } ) (initialState init)
      reactSpec' =
          Queue.whileMountedOne
            (allowReading setQueue)
            (\this x -> unsafeCoerceEff $ dispatcher this $ SetText x)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
