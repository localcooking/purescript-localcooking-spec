module LocalCooking.Spec.Snackbar where

import LocalCooking.Global.Error (GlobalError, printGlobalError)

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.Queue.WhileMounted as Queue
import Data.Nullable (toNullable)
import Data.Time.Duration (Milliseconds (..))
import Data.Maybe (Maybe (..))
import Data.Array as Array
import Data.Generic (class Generic, gShow)
import Control.Monad.Base (liftBase)
import Control.Monad.Aff (delay)
import Control.Monad.Eff.Uncurried (mkEffFn2, mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Class (liftEff)

import MaterialUI.Snackbar (snackbar)
import MaterialUI.IconButton (iconButton)
import MaterialUI.Icons.Close (closeIcon)

import Queue (READ)
import Queue.One as One





type State =
  { errors :: Array GlobalError
  , open :: Boolean
  }

initialState :: State
initialState =
  { errors: []
  , open: false
  }

data Action
  = GotMessage GlobalError
  | PopMessage

type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff. T.Spec (Effects eff) State Unit Action
spec = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      GotMessage x -> do
        -- liftEff $ unsafeCoerceEff $ log $ "got message: " <> show x
        void $ T.cotransform _ { errors = Array.snoc state.errors x, open = true }
      PopMessage -> do
        case Array.uncons state.errors of
          Nothing -> pure unit
          Just {head,tail} -> do
            void $ T.cotransform _ { open = false }
            liftBase $ delay $ Milliseconds 2000.0
            void $ T.cotransform _ { errors = tail }
            unless (Array.null tail) $ do
              liftBase $ delay $ Milliseconds 1000.0
              void $ T.cotransform _ { open = true }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ snackbar
        { open: state.open
        , autoHideDuration: toNullable $ Just $ Milliseconds 10000.0
        -- , resumeHideDuration: toNullable $ Just $ Milliseconds 0.0
        , onClose: mkEffFn2 \_ _ -> dispatch PopMessage
        , message: R.div []
          [ case Array.head state.errors of
              Nothing -> R.text ""
              Just e -> R.text (printGlobalError e)
          ]
        , action:
          [ iconButton
            { onTouchTap: mkEffFn1 \_ -> dispatch PopMessage
            } closeIcon
          ]
        }
      ]



messages :: forall eff
          . { globalErrorQueue :: One.Queue (read :: READ) (Effects eff) GlobalError
            } -> R.ReactElement
messages {globalErrorQueue} =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
      reactSpec' =
        Queue.whileMountedOne
          globalErrorQueue
          (\this x -> unsafeCoerceEff $ dispatcher this (GotMessage x))
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
