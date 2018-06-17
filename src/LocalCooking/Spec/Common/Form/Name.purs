module LocalCooking.Spec.Common.Form.Name where

import LocalCooking.Common.User.Name (Name, mkName)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Generic (class Generic, gEq)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
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
  { name :: String -- Represents the unvalidated state
  , rerender :: Unit
  }

initialState :: {initName :: String} -> State
initialState {initName} =
  { name: initName
  , rerender: unit
  }

data Action
  = ChangedName String
  | SetName NameState
  | NameUnfocused
  | ReRender

type Effects eff =
  ( ref :: REF
  | eff)


data NameState
  = NamePartial String
  | NameBad String
  | NameGood Name

derive instance genericNameState :: Generic NameState

instance eqNameState :: Eq NameState where
  eq = gEq



spec :: forall eff
      . { nameSignal   :: IxSignal (Effects eff) NameState
        , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
        , label        :: R.ReactElement
        , fullWidth    :: Boolean
        , name         :: String -- WARNING not semantically meaningful
                                 -- - for HTML node only
        , id           :: String
        } -> T.Spec (Effects eff) State Unit Action
spec
  { nameSignal
  , updatedQueue
  , label
  , fullWidth
  , name
  , id
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedName n -> do
        liftEff (IxSignal.set (NamePartial n) nameSignal)
        void $ T.cotransform _ { name = n }
      SetName x -> do
        liftEff (IxSignal.set x nameSignal)
        void $ T.cotransform _ { name = case x of
                                    NamePartial y -> y
                                    NameBad y -> y
                                    NameGood y -> show y
                               }
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit
      NameUnfocused -> do
        liftEff $ case mkName state.name of
          Nothing -> IxSignal.set (NameBad state.name) nameSignal
          Just n -> IxSignal.set (NameGood n) nameSignal
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit
      ReRender -> void $ T.cotransform _ { rerender = unit }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ textField
        { label
        , fullWidth
        , value: Input.valueString state.name
        , onChange: mkEffFn1 \e -> dispatch $ ChangedName (unsafeCoerce e).target.value
        , onBlur: mkEffFn1 \_ -> dispatch NameUnfocused
        , error: case unsafePerformEff (IxSignal.get nameSignal) of
          NamePartial _ -> false
          NameBad _ -> true
          NameGood _ -> false
        , name
        , id
        } []
      ]



name :: forall eff
      . { label        :: R.ReactElement
        , fullWidth    :: Boolean
        , name         :: String
        , id           :: String
        , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
        , nameSignal   :: IxSignal (Effects eff) NameState
        , setQueue     :: One.Queue (write :: WRITE) (Effects eff) NameState
        } -> R.ReactElement
name {label,fullWidth,name,id,updatedQueue,nameSignal,setQueue} =
  let init =
        { initName: case unsafePerformEff (IxSignal.get nameSignal) of
            NamePartial e -> e
            NameBad e -> e
            NameGood y -> show y
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { label
            , fullWidth
            , name
            , id
            , updatedQueue
            , nameSignal
            } ) (initialState init)
      reactSpec' =
          Queue.whileMountedOne
            (allowReading setQueue)
            (\this x -> unsafeCoerceEff $ dispatcher this $ SetName x)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
