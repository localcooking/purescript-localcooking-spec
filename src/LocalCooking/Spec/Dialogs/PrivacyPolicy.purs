module LocalCooking.Spec.Dialogs.PrivacyPolicy where

import LocalCooking.Spec.Dialogs.Generic (genericDialog)
import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Global.Links.Internal (PolicyLinks (..))
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks)
import LocalCooking.Global.Error (GlobalError)

import Prelude
import Data.URI.URI (print) as URI
import Data.URI.Location (class ToLocation, toLocation)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import React as R
import React.DOM as R
import React.DOM.Props as RP
import DOM (DOM)

import Queue (WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO



type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , dom       :: DOM
  | eff)



privacyPolicyDialog :: forall eff siteLinks userDetails userDetailsLinks
             . LocalCookingSiteLinks siteLinks userDetailsLinks
            => ToLocation siteLinks
            => LocalCookingParams siteLinks userDetails (Effects eff)
            -> { privacyPolicyDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Unit)
               }
            -> R.ReactElement
privacyPolicyDialog
  params@{toURI}
  { privacyPolicyDialogQueue
  } =
  genericDialog
  params
  { dialogQueue: privacyPolicyDialogQueue
  , closeQueue: Nothing
  , buttons: \_ -> []
  , title: "Privacy Policy"
  , submitValue: "Acknowledge"
  , pends: false
  , content:
    { component: \_ ->
      [ R.iframe
        [ RP.src $ URI.print $ toURI $ toLocation PrivacyPolicyLink
        , RP.style {width: "100%", border: "1px solid black"}
        ] []
      ]
    , obtain: \_ -> pure (Just unit)
    , reset: pure unit
    }
  }
