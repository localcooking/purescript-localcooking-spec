module LocalCooking.Spec.Common.Form.Search where

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
import MaterialUI.Input (inputAdornment)
import MaterialUI.Icons.Search (searchIcon)

import Unsafe.Coerce (unsafeCoerce)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue.Types (READ, WRITE, allowWriting, allowReading)
import Queue.One as One
import IxQueue (IxQueue)
import IxQueue as IxQueue



type State =
  { search :: String
  , rerender :: Unit
  }

initialState :: {initSearch :: String} -> State
initialState {initSearch} =
  { search: initSearch
  , rerender: unit
  }

data Action
  = ChangedSearch String
  | SetSearch String
  | SearchUnfocused
  | ReRender

type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff
      . { searchSignal :: IxSignal (Effects eff) String
        , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
        , label        :: R.ReactElement
        , fullWidth    :: Boolean
        , id           :: String
        } -> T.Spec (Effects eff) State Unit Action
spec
  { searchSignal
  , updatedQueue
  , label
  , fullWidth
  , id
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedSearch n -> do
        liftEff (IxSignal.set n searchSignal)
        void $ T.cotransform _ { search = n }
      SetSearch x -> do
        liftEff $ unsafeCoerceEff $ log $ "setting: " <> x
        liftEff (IxSignal.set x searchSignal)
        void $ T.cotransform _ { search = x }
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit
      SearchUnfocused -> do
        liftEff $ IxSignal.set state.search searchSignal
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit
      ReRender -> void $ T.cotransform _ { rerender = unit }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ textField
        { label
        , fullWidth
        , value: Input.valueString state.search
        , onChange: mkEffFn1 \e -> dispatch $ ChangedSearch (unsafeCoerce e).target.value
        , onBlur: mkEffFn1 \_ -> dispatch SearchUnfocused
        , name: id
        , id
        , "InputProps":
          { startAdornment:
            inputAdornment
              { position: Input.start
              }
              searchIcon
          }
        } []
      ]



search :: forall eff
        . { label        :: R.ReactElement
          , fullWidth    :: Boolean
          , id           :: String
          , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          , searchSignal :: IxSignal (Effects eff) String
          , setQueue     :: One.Queue (write :: WRITE) (Effects eff) String
          } -> R.ReactElement
search {label,fullWidth,id,updatedQueue,searchSignal,setQueue} =
  let init =
        { initSearch: unsafePerformEff (IxSignal.get searchSignal)
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { label
            , fullWidth
            , id
            , updatedQueue
            , searchSignal
            } ) (initialState init)
      reactSpec' =
          Queue.whileMountedOne
            (allowReading setQueue)
            (\this x -> unsafeCoerceEff $ dispatcher this $ SetSearch x)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
