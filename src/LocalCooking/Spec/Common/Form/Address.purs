module LocalCooking.Spec.Common.Form.Address where

import LocalCooking.Spec.Common.Form.Select (select)
import LocalCooking.Spec.Common.Form.Text (text)
import Data.Address (USAState, allUSAStates, usaStateParser, USAAddress (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Generic (class Generic, gEq)
import Data.Int.Parse (parseInt, toRadix)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (text) as R
import React.Queue.WhileMounted as Queue
import React.Signal.WhileMounted as Signal

import Unsafe.Coerce (unsafeCoerce)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue.Types (READ, WRITE, allowWriting, allowReading, readOnly, writeOnly)
import Queue.One as One
import IxQueue (IxQueue)
import IxQueue as IxQueue



type State =
  { rerender :: Unit
  }

initialState :: State
initialState =
  { rerender: unit
  }

data Action
--   = ChangedName String
--   | SetName NameState
--   | NameUnfocused
  = ReRender

type Effects eff =
  ( ref :: REF
  | eff)




spec :: forall eff
      . { street ::
          { updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          , signal :: IxSignal (Effects eff) String
          , setQueue :: One.Queue (write :: WRITE) (Effects eff) String
          }
        , city ::
          { updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          , signal :: IxSignal (Effects eff) String
          , setQueue :: One.Queue (write :: WRITE) (Effects eff) String
          }
        , state ::
          { signal :: IxSignal (Effects eff) (Maybe USAState)
          , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          }
        , zip ::
          { updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          , signal :: IxSignal (Effects eff) String
          , setQueue :: One.Queue (write :: WRITE) (Effects eff) String
          }
        } -> T.Spec (Effects eff) State Unit Action
spec
  { street
  , city
  , state
  , zip
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ReRender -> void $ T.cotransform _ { rerender = unit }

    render :: T.Render State Unit Action
    render dispatch props compState children =
      [ text
        { label: R.text "Street"
        , fullWidth: true
        , id: "street"
        , updatedQueue: street.updatedQueue
        , textSignal: street.signal
        , setQueue: street.setQueue
        }
      , text
        { label: R.text "City"
        , fullWidth: true
        , id: "city"
        , updatedQueue: city.updatedQueue
        , textSignal: city.signal
        , setQueue: city.setQueue
        }
      , select
        { entries: allUSAStates
        , parser: usaStateParser
        , entriesSignal: state.signal
        , updatedQueue: state.updatedQueue
        , label: "State"
        , id: "state"
        }
      , text
        { label: R.text "Zip"
        , fullWidth: true
        , id: "zip"
        , updatedQueue: zip.updatedQueue
        , textSignal: zip.signal
        , setQueue: zip.setQueue
        }
      ]



address :: forall eff
         . { updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
           , addressSignal :: IxSignal (Effects eff) USAAddress
           , setQueue :: One.Queue (write :: WRITE) (Effects eff) USAAddress
           } -> R.ReactElement
address {updatedQueue,addressSignal,setQueue} =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { street:
              { updatedQueue: streetUpdatedQueue
              , signal: streetSignal
              , setQueue: streetSetQueue
              }
            , city:
              { updatedQueue: cityUpdatedQueue
              , signal: citySignal
              , setQueue: citySetQueue
              }
            , state:
              { updatedQueue: stateUpdatedQueue
              , signal: stateSignal
              }
            , zip:
              { updatedQueue: zipUpdatedQueue
              , signal: zipSignal
              , setQueue: zipSetQueue
              }
            } ) initialState
      relaySignal = do
        street <- IxSignal.get streetSignal
        city <- IxSignal.get citySignal
        mState <- IxSignal.get stateSignal
        case mState of
          Nothing -> pure unit
          Just state -> do
            z <- IxSignal.get zipSignal
            case parseInt z (toRadix 10) of
              Nothing -> pure unit
              Just zip ->
                IxSignal.set (USAAddress {street,city,state,zip}) addressSignal
      reactSpec' =
          Queue.whileMountedIx
            streetUpdatedQueue
            "streetUpdated"
            (\_ _ -> IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit)
        $ Queue.whileMountedIx
            cityUpdatedQueue
            "cityUpdated"
            (\_ _ -> IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit)
        $ Queue.whileMountedIx
            stateUpdatedQueue
            "stateUpdated"
            (\_ _ -> IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit)
        $ Queue.whileMountedIx
            zipUpdatedQueue
            "zipUpdated"
            (\_ _ -> IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit)
        $ Signal.whileMountedIx
            streetSignal
            "streetSignal"
            (\_ _ -> relaySignal)
        $ Signal.whileMountedIx
            citySignal
            "citySignal"
            (\_ _ -> relaySignal)
        $ Signal.whileMountedIx
            stateSignal
            "stateSignal"
            (\_ _ -> relaySignal)
        $ Signal.whileMountedIx
            zipSignal
            "zipSignal"
            (\_ _ -> relaySignal)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
  where
    streetUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    streetSignal = unsafePerformEff $ IxSignal.make ""
    streetSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    cityUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    citySignal = unsafePerformEff $ IxSignal.make ""
    citySetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    stateSignal = unsafePerformEff $ IxSignal.make Nothing
    stateUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    zipUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    zipSignal = unsafePerformEff $ IxSignal.make ""
    zipSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
