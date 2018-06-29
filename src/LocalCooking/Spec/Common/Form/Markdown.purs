module LocalCooking.Spec.Common.Form.Markdown where

import Prelude
import Data.String.Markdown (MarkdownText (..))
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
  { markdown :: String
  , rerender :: Unit
  }

initialState :: {initMarkdown :: String} -> State
initialState {initMarkdown} =
  { markdown: initMarkdown
  , rerender: unit
  }

data Action
  = ChangedMarkdown String
  | SetMarkdown MarkdownText
  | MarkdownUnfocused
  | ReRender

type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff
      . { markdownSignal :: IxSignal (Effects eff) MarkdownText
        , updatedQueue   :: IxQueue (read :: READ) (Effects eff) Unit
        , label          :: R.ReactElement
        , fullWidth      :: Boolean
        , id             :: String
        } -> T.Spec (Effects eff) State Unit Action
spec
  { markdownSignal
  , updatedQueue
  , label
  , fullWidth
  , id
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedMarkdown n -> do
        liftEff $ IxSignal.set (MarkdownText n) markdownSignal
        void $ T.cotransform _ { markdown = n }
      SetMarkdown x -> do
        liftEff $ unsafeCoerceEff $ log $ "setting: " <> show x
        liftEff (IxSignal.set x markdownSignal)
        void $ T.cotransform _ { markdown = show x }
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit
      MarkdownUnfocused -> do
        liftEff $ IxSignal.set (MarkdownText state.markdown) markdownSignal
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit
      ReRender -> void $ T.cotransform _ { rerender = unit }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ textField
        { label
        , fullWidth
        , multiline: true
        , rows: 10
        , value: Input.valueString state.markdown
        , onChange: mkEffFn1 \e -> dispatch $ ChangedMarkdown (unsafeCoerce e).target.value
        , onBlur: mkEffFn1 \_ -> dispatch MarkdownUnfocused
        , name: id
        , id
        } []
      ]



markdown :: forall eff
      . { label          :: R.ReactElement
        , fullWidth      :: Boolean
        , id             :: String
        , updatedQueue   :: IxQueue (read :: READ) (Effects eff) Unit
        , markdownSignal :: IxSignal (Effects eff) MarkdownText
        , setQueue       :: One.Queue (write :: WRITE) (Effects eff) MarkdownText
        } -> R.ReactElement
markdown {label,fullWidth,id,updatedQueue,markdownSignal,setQueue} =
  let init =
        { initMarkdown: unsafePerformEff (show <$> IxSignal.get markdownSignal)
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { label
            , fullWidth
            , id
            , updatedQueue
            , markdownSignal
            } ) (initialState init)
      reactSpec' =
          Queue.whileMountedOne
            (allowReading setQueue)
            (\this x -> unsafeCoerceEff $ dispatcher this $ SetMarkdown x)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
