module LocalCooking.Spec.Common.Form.BlogPostCategory where

import LocalCooking.Common.Blog (BlogPostCategory (..))

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
  | SetBlogPostCategory BlogPostCategory
  | TextUnfocused
  | ReRender

type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff
      . { blogPostCategorySignal :: IxSignal (Effects eff) BlogPostCategory
        , updatedQueue           :: IxQueue (read :: READ) (Effects eff) Unit
        , label                  :: R.ReactElement
        , fullWidth              :: Boolean
        , id                     :: String
        } -> T.Spec (Effects eff) State Unit Action
spec
  { blogPostCategorySignal
  , updatedQueue
  , label
  , fullWidth
  , id
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedText n -> do
        liftEff (IxSignal.set (BlogPostCategory n) blogPostCategorySignal)
        void $ T.cotransform _ { text = n }
      SetBlogPostCategory x -> do
        liftEff (IxSignal.set x blogPostCategorySignal)
        void $ T.cotransform _ { text = show x }
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit
      TextUnfocused -> do
        liftEff $ IxSignal.set (BlogPostCategory state.text) blogPostCategorySignal
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



blogPostCategory :: forall eff
                  . { label                  :: R.ReactElement
                    , fullWidth              :: Boolean
                    , id                     :: String
                    , updatedQueue           :: IxQueue (read :: READ) (Effects eff) Unit
                    , blogPostCategorySignal :: IxSignal (Effects eff) BlogPostCategory
                    , setQueue               :: One.Queue (write :: WRITE) (Effects eff) BlogPostCategory
                    } -> R.ReactElement
blogPostCategory {label,fullWidth,id,updatedQueue,blogPostCategorySignal,setQueue} =
  let init =
        { initText: unsafePerformEff (show <$> IxSignal.get blogPostCategorySignal)
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { label
            , fullWidth
            , id
            , updatedQueue
            , blogPostCategorySignal
            } ) (initialState init)
      reactSpec' =
          Queue.whileMountedOne
            (allowReading setQueue)
            (\this x -> unsafeCoerceEff $ dispatcher this $ SetBlogPostCategory x)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
