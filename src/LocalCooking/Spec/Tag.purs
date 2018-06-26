module LocalCooking.Spec.Tag where

import LocalCooking.Common.Tag (Tag (..))
import LocalCooking.Common.Tag.Chef (ChefTag (..))
import LocalCooking.Common.Tag.Culture (CultureTag (..))
import LocalCooking.Common.Tag.Diet (DietTag (..))
import LocalCooking.Common.Tag.Farm (FarmTag (..))
import LocalCooking.Common.Tag.Ingredient (IngredientTag (..))
import LocalCooking.Common.Tag.Meal (MealTag (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (text) as R

import MaterialUI.Types (createStyles)
import MaterialUI.Chip (chip)
import MaterialUI.Chip as Chip



data AnyTag
  = TagChef ChefTag
  | TagCulture CultureTag
  | TagDiet DietTag
  | TagFarm FarmTag
  | TagIngredient IngredientTag
  | TagMeal MealTag



type State = Unit

initialState :: State
initialState = unit

data Action
  = Clicked
  | Deleted


type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff
      . { onClick :: Maybe (Eff (Effects eff) Unit)
        , onDelete :: Maybe (Eff (Effects eff) Unit)
        , tag :: AnyTag
        }
     -> T.Spec (Effects eff) State Unit Action
spec {onClick,onDelete,tag} = T.simpleSpec performAction render
  where
    performAction action props state = pure unit

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ Chip.withStyles
        (\theme ->
          { root: createStyles
            { margin: theme.spacing.unit `div` 2
            , background: case tag of
              TagDiet _ -> "#f4d941"
              TagIngredient _ -> "#45f441"
              TagMeal _ -> "#f441d4"
              TagChef _ -> ""
              TagCulture _ -> "#419ef4"
              TagFarm _ -> ""
            }
          }
        )
        \{classes} ->
        let label = R.text $ case tag of
              TagChef (ChefTag (Tag x)) -> x
              TagCulture (CultureTag (Tag x)) -> x
              TagDiet (DietTag (Tag x)) -> x
              TagIngredient (IngredientTag (Tag x)) -> x
              TagFarm (FarmTag (Tag x)) -> x
              TagMeal (MealTag (Tag x)) -> x
        in  case Tuple onClick onDelete of
              Tuple (Just onClick') (Just onDelete') -> chip
                { label
                , onClick: mkEffFn1 \_ -> unsafeCoerceEff onClick'
                , onDelete: mkEffFn1 \_ -> unsafeCoerceEff onDelete'
                , classes: Chip.createClasses classes
                }
              Tuple Nothing (Just onDelete') -> chip
                { label
                , onDelete: mkEffFn1 \_ -> unsafeCoerceEff onDelete'
                , classes: Chip.createClasses classes
                }
              Tuple (Just onClick') Nothing -> chip
                { label
                , onClick: mkEffFn1 \_ -> unsafeCoerceEff onClick'
                , classes: Chip.createClasses classes
                }
              Tuple Nothing Nothing -> chip
                { label
                , classes: Chip.createClasses classes
                }
      ]




tag :: forall eff
     . { tag :: AnyTag
       , onClick :: Maybe (Eff (Effects eff) Unit)
       , onDelete :: Maybe (Eff (Effects eff) Unit)
       } -> R.ReactElement
tag params =
  let {spec: reactSpec, dispatcher} = T.createReactSpec (spec params) initialState
  in  R.createElement (R.createClass reactSpec) unit []
