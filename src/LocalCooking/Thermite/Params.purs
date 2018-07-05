module LocalCooking.Thermite.Params where

import LocalCooking.Common.AccessToken.Auth (AuthToken)

import Prelude
import Data.Maybe (Maybe)
import Data.URI.URI (URI)
import Data.URI.Location (Location)
import Data.Lens (Lens', (%~))
import Data.UUID (GENUUID)
import Data.Generic (class Generic, gShow)
import Data.String.Yarn (unlines)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal

import React (ReactSpec, ReactThis)
import React.Signal.WhileMounted as Signal
import DOM.HTML.Window.Extra (WindowSize)
import Thermite as T



-- | Universal arguments for each React component process
type LocalCookingParams siteLinks userDetails eff =
  { toURI             :: Location -> URI
  , siteLinks         :: siteLinks -> Eff eff Unit
  , currentPageSignal :: IxSignal eff siteLinks
  , windowSizeSignal  :: IxSignal eff WindowSize
  , authTokenSignal   :: IxSignal eff (Maybe AuthToken)
  , userDetailsSignal :: IxSignal eff (Maybe userDetails)
  }


-- | Storable state for each Thermite action schema
type LocalCookingState siteLinks userDetails =
  { currentPage :: siteLinks
  , windowSize  :: WindowSize
  , authToken   :: Maybe AuthToken
  , userDetails :: Maybe userDetails
  }

-- | View-only data components
type LocalCookingStateLight siteLinks =
  { currentPage :: siteLinks
  , windowSize  :: WindowSize
  }


showLocalCookingState :: forall siteLinks userDetails
                       . Show siteLinks
                      => Show userDetails
                      => LocalCookingState siteLinks userDetails
                      -> String
showLocalCookingState {currentPage,windowSize,authToken,userDetails} =
  unlines
  [ "{ currentPage: " <> show currentPage
  , ", windowSize: " <> show windowSize
  , ", authToken: " <> show authToken
  , ", userDetails: " <> show userDetails
  , "}"
  ]

showLocalCookingStateLight :: forall siteLinks
                            . Show siteLinks
                           => LocalCookingStateLight siteLinks
                           -> String
showLocalCookingStateLight {currentPage,windowSize} =
  unlines
  [ "{ currentPage: " <> show currentPage
  , ", windowSize: " <> show windowSize
  , "}"
  ]



-- | Obtain state from params
initLocalCookingState :: forall siteLinks userDetails eff
                       . LocalCookingParams siteLinks userDetails (ref :: REF | eff)
                      -> Eff (ref :: REF | eff) (LocalCookingState siteLinks userDetails)
initLocalCookingState {currentPageSignal,windowSizeSignal,authTokenSignal,userDetailsSignal} = do
  currentPage <- IxSignal.get currentPageSignal
  windowSize <- IxSignal.get windowSizeSignal
  authToken <- IxSignal.get authTokenSignal
  userDetails <- IxSignal.get userDetailsSignal
  pure
    { currentPage
    , windowSize
    , authToken
    , userDetails
    }

initLocalCookingStateLight :: forall siteLinks userDetails eff
                            . LocalCookingParams siteLinks userDetails (ref :: REF | eff)
                           -> Eff (ref :: REF | eff) (LocalCookingStateLight siteLinks)
initLocalCookingStateLight {currentPageSignal,windowSizeSignal} = do
  currentPage <- IxSignal.get currentPageSignal
  windowSize <- IxSignal.get windowSizeSignal
  pure
    { currentPage
    , windowSize
    }


-- | Modifications to state
data LocalCookingAction siteLinks userDetails
  = ChangedCurrentPage siteLinks
  | ChangedWindowSize WindowSize
  | ChangedAuthToken (Maybe AuthToken)
  | ChangedUserDetails (Maybe userDetails)

derive instance genericLocalCookingAction :: (Generic siteLinks, Generic userDetails) => Generic (LocalCookingAction siteLinks userDetails)

instance showLocalCookingAction :: (Generic siteLinks, Generic userDetails) => Show (LocalCookingAction siteLinks userDetails) where
  show = gShow


data LocalCookingActionLight siteLinks
  = ChangedCurrentPageLight siteLinks
  | ChangedWindowSizeLight WindowSize

derive instance genericLocalCookingActionLight :: (Generic siteLinks) => Generic (LocalCookingActionLight siteLinks)

instance showLocalCookingActionLight :: (Generic siteLinks) => Show (LocalCookingActionLight siteLinks) where
  show = gShow


type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)


-- | Enact modifications to state
performActionLocalCooking :: forall siteLinks userDetails eff state props
                           . Lens' state (LocalCookingState siteLinks userDetails)
                          -> T.PerformAction eff state props (LocalCookingAction siteLinks userDetails)
performActionLocalCooking getLCState a props state =
  let go :: LocalCookingState siteLinks userDetails -> LocalCookingState siteLinks userDetails
      go = case a of
        ChangedCurrentPage x -> _ { currentPage = x }
        ChangedWindowSize x -> _ { windowSize = x }
        ChangedAuthToken x -> _ { authToken = x }
        ChangedUserDetails x -> _ { userDetails = x }
  in  void (T.cotransform (getLCState %~ go))


performActionLocalCookingLight :: forall siteLinks eff state props
                                . Lens' state (LocalCookingStateLight siteLinks)
                               -> T.PerformAction eff state props (LocalCookingActionLight siteLinks)
performActionLocalCookingLight getLCState a props state =
  let go :: LocalCookingStateLight siteLinks -> LocalCookingStateLight siteLinks
      go = case a of
        ChangedCurrentPageLight x -> _ { currentPage = x }
        ChangedWindowSizeLight x -> _ { windowSize = x }
  in  void (T.cotransform (getLCState %~ go))


-- | Bind Thermite action modifications to the React component signals
whileMountedLocalCooking :: forall siteLinks userDetails eff state action props render
                          . Eq siteLinks
                         => Eq userDetails
                         => LocalCookingParams siteLinks userDetails (Effects eff)
                         -> String
                         -> (LocalCookingAction siteLinks userDetails -> action)
                         -> (ReactThis props state -> action -> Eff (Effects eff) Unit)
                         -> ReactSpec props state render (Effects eff)
                         -> ReactSpec props state render (Effects eff)
whileMountedLocalCooking
  { currentPageSignal
  , windowSizeSignal
  , authTokenSignal
  , userDetailsSignal
  }
  k
  buildLCAction
  dispatcher
  reactSpec
  = Signal.whileMountedIxDiff
      currentPageSignal
      k
      (\this x -> dispatcher this (buildLCAction (ChangedCurrentPage x)))
  $ Signal.whileMountedIxDiff
      windowSizeSignal
      k
      (\this x -> dispatcher this (buildLCAction (ChangedWindowSize x)))
  $ Signal.whileMountedIxDiff
      authTokenSignal
      k
      (\this x -> dispatcher this (buildLCAction (ChangedAuthToken x)))
  $ Signal.whileMountedIxDiff
      userDetailsSignal
      k
      (\this x -> dispatcher this (buildLCAction (ChangedUserDetails x)))
      reactSpec


whileMountedLocalCookingLight :: forall siteLinks userDetails eff state action props render
                               . Eq siteLinks
                              => LocalCookingParams siteLinks userDetails (Effects eff)
                              -> String
                              -> (LocalCookingActionLight siteLinks -> action)
                              -> (ReactThis props state -> action -> Eff (Effects eff) Unit)
                              -> ReactSpec props state render (Effects eff)
                              -> ReactSpec props state render (Effects eff)
whileMountedLocalCookingLight
  { currentPageSignal
  , windowSizeSignal
  }
  k
  buildLCAction
  dispatcher
  reactSpec
  = Signal.whileMountedIxDiff
      currentPageSignal
      k
      (\this x -> dispatcher this (buildLCAction (ChangedCurrentPageLight x)))
  $ Signal.whileMountedIxDiff
      windowSizeSignal
      k
      (\this x -> dispatcher this (buildLCAction (ChangedWindowSizeLight x)))
      reactSpec
