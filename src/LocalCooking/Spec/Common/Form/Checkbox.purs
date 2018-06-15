module LocalCooking.Spec.Common.Form.Checkbox where

import Prelude
import Data.Set (Set)
import Data.Set as Set
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Uncurried (mkEffFn2)

import Thermite as T
import React as R
import React.DOM as R

import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Checkbox (checkbox)
import MaterialUI.Form (formControl, formGroup, formLabel, formControlLabel)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



type State a = Set a

data Action a
  = ClickedEntry a Boolean

type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff a
      . Ord a
     => Show a
     => { entriesSignal :: IxSignal (Effects eff) (Array a)
        , label :: String
        , entries :: Array a
        }
     -> T.Spec (Effects eff) (State a) Unit (Action a)
spec
  { entriesSignal
  , label
  , entries
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ClickedEntry x on -> do
        let state' state
              | on = Set.insert x state
              | otherwise = Set.delete x state
        liftEff $ IxSignal.set (Set.toUnfoldable (state' state)) entriesSignal
        void (T.cotransform state')

    render :: T.Render (State a) Unit (Action a)
    render dispatch props state children =
      [ formControl
        { component: R.createClassStateless' \_ children ->
          [R.fieldset [] children]
        }
        [ formLabel
          { component: R.createClassStateless' \_ children ->
            [R.legend [] children]
          }
          [typography {variant: Typography.body1} [R.text label]]
        , formGroup {} $
          let role :: a -> R.ReactElement
              role x =
                formControlLabel
                { control:
                  checkbox
                  { checked: Set.member x state
                  , onChange: mkEffFn2 \_ on -> dispatch (ClickedEntry x on)
                  , value: (show x)
                  } []
                , label: R.text (show x)
                }
          in  map role entries
        ]
      ]


checkboxes :: forall eff a
            . Ord a
           => Show a
           => { entriesSignal :: IxSignal (Effects eff) (Array a)
              , label :: String
              , entries :: Array a
              }
           -> R.ReactElement
checkboxes args@{entriesSignal} =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec (spec args)
          (unsafePerformEff $ Set.fromFoldable <$> IxSignal.get entriesSignal)
  in  R.createElement (R.createClass reactSpec) unit []
