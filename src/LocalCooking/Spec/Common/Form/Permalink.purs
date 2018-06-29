module LocalCooking.Spec.Common.Form.Permalink where

import Prelude
import Data.Either (Either (..))
import Data.String.Permalink (Permalink, permalinkParser)
import Data.Generic (class Generic, gEq)
import Text.Parsing.StringParser (runParser)
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



-- TODO bind to validation queues


type State =
  { permalink :: String
  , rerender :: Unit
  }

initialState :: {initPermalink :: String} -> State
initialState {initPermalink} =
  { permalink: initPermalink
  , rerender: unit
  }

data Action
  = ChangedPermalink String
  | SetPermalink PermalinkState
  | PermalinkUnfocused
  | ReRender

type Effects eff =
  ( ref :: REF
  | eff)


data PermalinkState
  = PermalinkPartial String
  | PermalinkBad String
  | PermalinkGood Permalink

derive instance genericPermalinkState :: Generic PermalinkState

instance eqPermalinkState :: Eq PermalinkState where
  eq = gEq



spec :: forall eff
      . { permalinkSignal :: IxSignal (Effects eff) PermalinkState
        , updatedQueue    :: IxQueue (read :: READ) (Effects eff) Unit
        , label           :: R.ReactElement
        , fullWidth       :: Boolean
        , id              :: String
        } -> T.Spec (Effects eff) State Unit Action
spec
  { permalinkSignal
  , updatedQueue
  , label
  , fullWidth
  , id
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedPermalink n -> do
        liftEff (IxSignal.set (PermalinkPartial n) permalinkSignal)
        void $ T.cotransform _ { permalink = n }
      SetPermalink x -> do
        liftEff (IxSignal.set x permalinkSignal)
        void $ T.cotransform _ { permalink = case x of
                                    PermalinkPartial y -> y
                                    PermalinkBad y -> y
                                    PermalinkGood y -> show y
                               }
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit
      PermalinkUnfocused -> do
        liftEff $ case runParser permalinkParser state.permalink of
          Left _ -> IxSignal.set (PermalinkBad state.permalink) permalinkSignal
          Right x -> IxSignal.set (PermalinkGood x) permalinkSignal
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit
      ReRender -> void $ T.cotransform _ { rerender = unit }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ textField
        { label
        , fullWidth
        , value: Input.valueString state.permalink
        , onChange: mkEffFn1 \e -> dispatch $ ChangedPermalink (unsafeCoerce e).target.value
        , onBlur: mkEffFn1 \_ -> dispatch PermalinkUnfocused
        , name: id
        , error: case unsafePerformEff (IxSignal.get permalinkSignal) of
          PermalinkPartial _ -> false
          PermalinkBad _ -> true
          PermalinkGood _ -> false
        , id
        } []
      ]



permalink :: forall eff
           . { label           :: R.ReactElement
             , fullWidth       :: Boolean
             , id              :: String
             , updatedQueue    :: IxQueue (read :: READ) (Effects eff) Unit
             , permalinkSignal :: IxSignal (Effects eff) PermalinkState
             , setQueue        :: One.Queue (write :: WRITE) (Effects eff) PermalinkState
             } -> R.ReactElement
permalink {label,fullWidth,id,updatedQueue,permalinkSignal,setQueue} =
  let init =
        { initPermalink: case unsafePerformEff (IxSignal.get permalinkSignal) of
            PermalinkPartial x -> x
            PermalinkBad x -> x
            PermalinkGood x -> show x
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { label
            , fullWidth
            , id
            , updatedQueue
            , permalinkSignal
            } ) (initialState init)
      reactSpec' =
          Queue.whileMountedOne
            (allowReading setQueue)
            (\this x -> unsafeCoerceEff $ dispatcher this $ SetPermalink x)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
