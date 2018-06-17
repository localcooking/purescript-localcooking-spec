module LocalCooking.Spec.Common.Form.Select where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Generic (class Generic, gEq)
import Data.Set (Set)
import Data.Set as Set
import Text.Parsing.StringParser (Parser, runParser)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (text) as R
import React.Queue.WhileMounted as Queue

import MaterialUI.Types (createStyles)
import MaterialUI.Select as Select
import MaterialUI.Input as Input
import MaterialUI.Form (formControl, formGroup, formLabel, formControlLabel)
import MaterialUI.Menu (menuItem)

import Unsafe.Coerce (unsafeCoerce)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue.Types (READ, WRITE, allowWriting, allowReading)
import Queue.One as One
import IxQueue (IxQueue)
import IxQueue as IxQueue



type State a = Maybe a

data Action a
  = ClickedEntry Input.Value -- Boolean
  -- | SetName NameState
  -- | NameUnfocused
  -- | ReRender

type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff a
      . Show a
     => Eq a
     => { entriesSignal :: IxSignal (Effects eff) (Maybe a)
        , updatedQueue  :: IxQueue (read :: READ) (Effects eff) Unit
        , label         :: String
        , entries       :: Array a
        , id            :: String
        , parser        :: Parser a
        , fullWidth     :: Boolean
        } -> T.Spec (Effects eff) (State a) Unit (Action a)
spec
  { entriesSignal
  , updatedQueue
  , label
  , entries
  , id
  , parser
  , fullWidth
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ClickedEntry v -> do
        let mx = case Input.readValue v of
                  Left _ -> Nothing
                  Right rv -> case rv of
                    Input.ValueNull -> Nothing
                    Input.ValueString s -> case runParser parser s of
                      Left _ -> Nothing
                      Right x -> Just x
                    _ -> Nothing
        when (mx /= state) $ do
          liftEff $ IxSignal.set mx entriesSignal
          void $ T.cotransform \_ -> mx
          liftEff $ IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit

    render :: T.Render (State a) Unit (Action a)
    render dispatch props state children =
      [ formControl {}
        [ Input.inputLabel {htmlFor: id} [R.text label]
        , Select.select
          { value: case state of
              Nothing -> Input.valueNull
              Just x -> Input.valueString (show x)
          , onChange: mkEffFn1 \e -> dispatch $ ClickedEntry (unsafeCoerce e).target.value
          , inputProps:
            { name: id
            , id
            }
          , style: if fullWidth then createStyles {width: "100%"} else createStyles {}
          } $ map (\e -> menuItem
                    { value: Input.valueString (show e)
                    } [R.text (show e)]
                  ) entries
        ]
      ]



select :: forall eff a
        . Show a
       => Eq a
       => { entries       :: Array a
          , parser        :: Parser a
          , entriesSignal :: IxSignal (Effects eff) (Maybe a)
          , updatedQueue  :: IxQueue (read :: READ) (Effects eff) Unit
          , label         :: String
          , id            :: String
          , fullWidth     :: Boolean
          } -> R.ReactElement
select args@{entriesSignal} =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          (spec args) (unsafePerformEff $ IxSignal.get entriesSignal)
  in  R.createElement (R.createClass reactSpec) unit []
