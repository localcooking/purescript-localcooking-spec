module LocalCooking.Spec.Common.Form.BlogPostPriority where

import LocalCooking.Common.Blog (BlogPostPriority (..))

import Prelude
import Data.Int (round, toNumber)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.Queue.WhileMounted as Queue

import MaterialUI.Input (input, inputLabel)
import MaterialUI.Input as Input
import MaterialUI.Form (formControl)

import Unsafe.Coerce (unsafeCoerce)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue.Types (READ, WRITE, allowWriting, allowReading)
import Queue.One as One
import IxQueue (IxQueue)
import IxQueue as IxQueue



type State =
  { value :: Number
  , rerender :: Unit
  }

initialState :: {initValue :: Number} -> State
initialState {initValue} =
  { value: initValue
  , rerender: unit
  }

data Action
  = ChangedValue Number
  | SetBlogPostPriority BlogPostPriority
  | ValueUnfocused
  | ReRender

type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff
      . { blogPostPrioritySignal :: IxSignal (Effects eff) BlogPostPriority
        , updatedQueue           :: IxQueue (read :: READ) (Effects eff) Unit
        , label                  :: R.ReactElement
        , fullWidth              :: Boolean
        , id                     :: String
        } -> T.Spec (Effects eff) State Unit Action
spec
  { blogPostPrioritySignal
  , updatedQueue
  , label
  , fullWidth
  , id
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedValue n -> do
        liftEff (IxSignal.set (BlogPostPriority (round n)) blogPostPrioritySignal)
        void $ T.cotransform _ { value = n }
      SetBlogPostPriority x@(BlogPostPriority n) -> do
        liftEff (IxSignal.set x blogPostPrioritySignal)
        void $ T.cotransform _ { value = toNumber n }
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit
      ValueUnfocused -> do
        liftEff $ IxSignal.set (BlogPostPriority (round state.value)) blogPostPrioritySignal
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit
      ReRender -> void $ T.cotransform _ { rerender = unit }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ formControl {fullWidth}
        [ inputLabel {htmlFor: "blogPostPriority"} [label]
        , input
          { id
          , value: Input.valueNumber state.value
          , onChange: mkEffFn1 \e -> dispatch $ ChangedValue (unsafeCoerce e).target.value
          , onBlur: mkEffFn1 \_ -> dispatch ValueUnfocused
          , name: id
          } []
        ]
      ]



blogPostPriority :: forall eff
      . { label                  :: R.ReactElement
        , fullWidth              :: Boolean
        , id                     :: String
        , updatedQueue           :: IxQueue (read :: READ) (Effects eff) Unit
        , blogPostPrioritySignal :: IxSignal (Effects eff) BlogPostPriority
        , setQueue               :: One.Queue (write :: WRITE) (Effects eff) BlogPostPriority
        } -> R.ReactElement
blogPostPriority {label,fullWidth,id,updatedQueue,blogPostPrioritySignal,setQueue} =
  let init =
        { initValue: unsafePerformEff ((\(BlogPostPriority i) -> toNumber i) <$> IxSignal.get blogPostPrioritySignal)
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { label
            , fullWidth
            , id
            , updatedQueue
            , blogPostPrioritySignal
            } ) (initialState init)
      reactSpec' =
          Queue.whileMountedOne
            (allowReading setQueue)
            (\this x -> unsafeCoerceEff $ dispatcher this $ SetBlogPostPriority x)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
