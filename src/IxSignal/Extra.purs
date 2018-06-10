module IxSignal.Extra where
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.UUID (GENUUID, genUUID)
import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF)


onWhenIx :: forall eff a b
          . (b -> Maybe a)
         -> (a -> Eff (ref :: REF | eff) Unit)
         -> String
         -> IxSignal (ref :: REF | eff) b
         -> Eff (ref :: REF | eff) Unit
onWhenIx g f k sig = do
  IxSignal.subscribeIx go k sig
  where
    go b = case g b of
      Nothing -> pure unit
      Just x -> do
        IxSignal.deleteSubscriber k sig
        f x

onWhen :: forall eff a b
        . (b -> Maybe a)
       -> (a -> Eff (ref :: REF, uuid :: GENUUID | eff) Unit)
       -> IxSignal (ref :: REF, uuid :: GENUUID | eff) b
       -> Eff (ref :: REF, uuid :: GENUUID | eff) Unit
onWhen g f sig = do
  k <- show <$> genUUID
  onWhenIx g f k sig


-- | Applies the handler once
onAvailableIx :: forall eff a
               . (a -> Eff (ref :: REF | eff) Unit)
              -> String
              -> IxSignal (ref :: REF | eff) (Maybe a)
              -> Eff (ref :: REF | eff) Unit
onAvailableIx = onWhenIx id


onAvailable :: forall eff a
             . (a -> Eff (ref :: REF, uuid :: GENUUID | eff) Unit)
            -> IxSignal (ref :: REF, uuid :: GENUUID | eff) (Maybe a)
            -> Eff (ref :: REF, uuid :: GENUUID | eff) Unit
onAvailable = onWhen id


getWhenIx :: forall eff a b
           . (b -> Maybe a)
          -> String
          -> IxSignal (ref :: REF | eff) b
          -> Aff (ref :: REF | eff) a
getWhenIx g k sig =
  makeAff \resolve -> do
    onWhenIx g (resolve <<< Right) k sig
    pure nonCanceler


getWhen :: forall eff a b
         . (b -> Maybe a)
        -> IxSignal (ref :: REF, uuid :: GENUUID | eff) b
        -> Aff (ref :: REF, uuid :: GENUUID | eff) a
getWhen g sig = do
  k <- show <$> liftEff genUUID
  getWhenIx g k sig


getAvailableIx :: forall eff a
                . String
               -> IxSignal (ref :: REF | eff) (Maybe a)
               -> Aff (ref :: REF | eff) a
getAvailableIx = getWhenIx id


getAvailable :: forall eff a
              . IxSignal (ref :: REF, uuid :: GENUUID | eff) (Maybe a)
             -> Aff (ref :: REF, uuid :: GENUUID | eff) a
getAvailable sig = do
  k <- show <$> liftEff genUUID
  getAvailableIx k sig
