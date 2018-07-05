module LocalCooking.Spec.Dialogs.Generic where

import LocalCooking.Spec.Common.Pending (pending)
import LocalCooking.Spec.Common.Form.Submit as Submit
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingActionLight, LocalCookingStateLight, performActionLocalCookingLight, whileMountedLocalCookingLight, initLocalCookingStateLight)
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks)

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Data.URI.Location (class ToLocation)
import Data.Time.Duration (Milliseconds (..))
import Data.Lens (Lens', lens)
import Control.Monad.Base (liftBase)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF, newRef, writeRef, readRef)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (text) as R
import React.Queue.WhileMounted as Queue
import DOM (DOM)
import DOM.HTML.Window.Extra (WindowSize (..))

import MaterialUI.Types (createStyles)
import MaterialUI.Dialog (dialog)
import MaterialUI.DialogContent (dialogContent)
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogActions (dialogActions)
import MaterialUI.Button (button)
import MaterialUI.Button as Button

import Queue.Types (readOnly, allowReading)
import Queue (READ, WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxQueue (IxQueue)
import IxQueue as IxQueue
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



type State input siteLinks =
  { open         :: Maybe input
  , localCooking :: LocalCookingStateLight siteLinks
  }


initialState :: forall input siteLinks
              . LocalCookingStateLight siteLinks -> State input siteLinks
initialState localCooking =
  { open: Nothing
  , localCooking
  }


data Action input siteLinks
  = Open input
  | Close
  | Submit
  | LocalCookingAction (LocalCookingActionLight siteLinks)

type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , dom       :: DOM
  | eff)


getLCState :: forall input siteLinks
            . Lens' (State input siteLinks) (LocalCookingStateLight siteLinks)
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff siteLinks userDetails userDetailsLinks input output
      . LocalCookingSiteLinks siteLinks userDetailsLinks
     => ToLocation siteLinks
     => LocalCookingParams siteLinks userDetails (Effects eff)
     -> { dialogOutputQueue :: One.Queue (write :: WRITE) (Effects eff) (Maybe output)
        , closeQueue :: Maybe (One.Queue (write :: WRITE) (Effects eff) Unit)
        , content ::
          { component ::
            { submitDisabled :: Boolean -> Eff (Effects eff) Unit
            , input          :: input
            } -> Array R.ReactElement
          , obtain    :: input -> Aff (Effects eff) (Maybe output)
          , reset     :: Eff (Effects eff) Unit
          }
        , buttons ::
          { close :: Eff (Effects eff) Unit
          , input :: input
          } -> Array R.ReactElement
        , title :: input -> String
        , submit ::
          { disabledSignal :: IxSignal (Effects eff) Boolean
          , queue          :: IxQueue (read :: READ) (Effects eff) Unit
          , value          :: Maybe String
          }
        , pendingSignal :: Maybe (IxSignal (Effects eff) Boolean)
        }
     -> T.Spec (Effects eff) (State input siteLinks) Unit (Action input siteLinks)
spec
  {toURI}
  { submit
  , content
  , pendingSignal
  , dialogOutputQueue
  , closeQueue
  , buttons
  , title
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      Open x -> void $ T.cotransform _ { open = Just x }
      Close -> do
        case pendingSignal of
          Nothing -> pure unit
          Just p  -> liftEff (IxSignal.set false p)
        void $ T.cotransform _ { open = Nothing }
        liftBase $ delay $ Milliseconds 2000.0
        liftEff content.reset
      Submit -> do
        case state.open of
          Nothing -> pure unit
          Just input -> do
            case pendingSignal of
              Nothing -> pure unit
              Just p -> liftEff (IxSignal.set true p)
            mOutput <- liftBase (content.obtain input)
            case mOutput of
              Nothing ->
                case pendingSignal of
                  Nothing -> pure unit
                  Just p -> liftEff (IxSignal.set false p)
              Just output -> do
                liftEff (One.putQueue dialogOutputQueue (Just output))
                case closeQueue of
                  Nothing -> performAction Close props state
                  Just closeQueue' -> pure unit
      LocalCookingAction a -> performActionLocalCookingLight getLCState a props state

    render :: T.Render (State input siteLinks) Unit (Action input siteLinks)
    render dispatch props state children =
      [ let dialog' =
              if state.localCooking.windowSize < Laptop
              then
                dialog
                  { open: case state.open of
                      Nothing -> false
                      Just _ -> true
                  , fullScreen: true
                  }
              else
                dialog
                  { open: case state.open of
                      Nothing -> false
                      Just _ -> true
                  , fullWidth: true
                  , onClose: mkEffFn1 \_ -> do
                      pending <- do
                        case pendingSignal of
                          Nothing -> pure false
                          Just p  -> unsafeCoerceEff (IxSignal.get p)
                      when (not pending) $ do
                        unsafeCoerceEff (One.putQueue dialogOutputQueue Nothing)
                        dispatch Close
                  }
        in  dialog' $ case state.open of
              Nothing -> []
              Just input ->
                [ dialogTitle {} [R.text (title input)]
                , dialogContent {style: createStyles {position: "relative"}} $
                    ( content.component
                      { submitDisabled: \d -> IxSignal.set d submit.disabledSignal
                      , input
                      }
                    ) <>
                      case pendingSignal of
                        Nothing -> []
                        Just p  ->
                          [ pending
                            { pendingSignal: p
                            }
                          ]
                , dialogActions {} $
                    buttons
                    { close: do
                        unsafeCoerceEff (dispatch Close)
                        One.putQueue dialogOutputQueue Nothing
                    , input
                    } <>
                    ( let cancelButton = button
                            { color: Button.default
                            , onTouchTap: mkEffFn1 \_ -> do
                                unsafeCoerceEff (One.putQueue dialogOutputQueue Nothing)
                                dispatch Close
                            } [R.text "Cancel"]
                      in  case submit.value of
                            Nothing -> [cancelButton]
                            Just submitValue ->
                              [ Submit.submit
                                { color: Button.primary
                                , variant: Button.flat
                                , size: Button.medium
                                , style: createStyles {}
                                , triggerQueue: submit.queue
                                , disabledSignal: submit.disabledSignal
                                , fullWidth: false
                                } [R.text submitValue]
                              , cancelButton
                              ]
                    )
                ]
      ]



genericDialog :: forall eff siteLinks userDetails userDetailsLinks input output
               . LocalCookingSiteLinks siteLinks userDetailsLinks
              => Eq siteLinks
              => ToLocation siteLinks
              => LocalCookingParams siteLinks userDetails (Effects eff)
              -> { dialogQueue       :: OneIO.IOQueues (Effects eff) input (Maybe output)
                 , closeQueue        :: Maybe (One.Queue (write :: WRITE) (Effects eff) Unit)
                 , buttons           ::
                    { close :: Eff (Effects eff) Unit
                    , input :: input
                    } -> Array R.ReactElement
                 , title             :: input -> String
                 , submitValue       :: Maybe String
                 , pends             :: Boolean
                 , content ::
                   { component ::
                     { submitDisabled :: Boolean -> Eff (Effects eff) Unit
                     , input          :: input
                     } -> Array R.ReactElement
                   , obtain    :: input -> Aff (Effects eff) (Maybe output)
                   , reset     :: Eff (Effects eff) Unit
                   }
                 }
              -> R.ReactElement
genericDialog
  params
  { dialogQueue: OneIO.IOQueues {input: dialogInputQueue, output: dialogOutputQueue}
  , closeQueue
  , content
  , submitValue
  , buttons
  , title
  , pends
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            params
            { closeQueue
            , buttons
            , title
            , submit:
              { queue: submitQueue
              , disabledSignal: submitDisabledSignal
              , value: submitValue
              }
            , content
            , pendingSignal
            , dialogOutputQueue
            } )
          (initialState (unsafePerformEff (initLocalCookingStateLight params)))
      _ = unsafePerformEff $ do
            case closeQueue of
              Nothing -> pure unit
              Just closeQueue' ->
                One.onQueue (allowReading closeQueue') $ \_ -> case innerCloseQueue of
                  Nothing -> pure unit
                  Just innerCloseQueue' -> do
                    -- relay to inner close only when mounted - drain otherwise
                    isMounted <- readRef isMountedRef
                    when isMounted $ One.putQueue innerCloseQueue' unit
      reactSpec' =
          whileMountedLocalCookingLight
            params
            "LocalCooking.Spec.Dialogs.Generic"
            LocalCookingAction
            (\this -> unsafeCoerceEff <<< dispatcher this)
        $ Queue.whileMountedOne
            dialogInputQueue
            (\this x -> unsafeCoerceEff $ dispatcher this $ Open x)
        $ ( case innerCloseQueue of
              Nothing -> id
              Just innerCloseQueue' ->
                Queue.whileMountedOne
                  innerCloseQueue'
                  (\this _ -> unsafeCoerceEff $ dispatcher this Close)
          )
        $ Queue.whileMountedIx
            submitQueue
            "onSubmit"
            (\this _ -> unsafeCoerceEff $ dispatcher this Submit)
        $   reactSpec
            { componentDidMount = \_ -> writeRef isMountedRef true
            , componentWillUnmount = \_ -> writeRef isMountedRef false
            }
  in  R.createElement (R.createClass reactSpec') unit []
  where
    submitDisabledSignal = unsafePerformEff $ IxSignal.make false
    pendingSignal = if pends then unsafePerformEff (Just <$> IxSignal.make false) else Nothing
    submitQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    -- replicate close queue, to drain when not mounted
    innerCloseQueue = unsafePerformEff $ case closeQueue of
      Nothing -> pure Nothing
      Just _ -> Just <$> One.newQueue
    isMountedRef = unsafePerformEff $ newRef false
