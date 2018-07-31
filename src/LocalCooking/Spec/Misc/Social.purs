module LocalCooking.Spec.Misc.Social where

import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Spec.FormData (FacebookLoginUnsavedFormData)
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Semantics.Common (SocialLoginForm (..))
import LocalCooking.Global.Links.External (ThirdPartyLoginReturnLinks (..))
import Facebook.Types (FacebookClientId)
import Facebook.State (FacebookLoginState (..))
import Facebook.Call (FacebookLoginLink (..), facebookLoginLinkToURI)

import Prelude
import Data.Maybe (Maybe (..), isJust)
import Data.URI.URI (print) as URI
import Data.URI.Location (class ToLocation, toLocation)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import React as R
import React.Icons (facebookIcon, twitterIcon, googleIcon)
import MaterialUI.Types (createStyles)
import MaterialUI.Button (button)
import MaterialUI.Button as Button

import IxSignal.Internal as IxSignal





-- | For social logins
mkSocialFab :: FacebookClientId -> String -> String -> R.ReactElement
            -> Boolean
            -> Maybe (FacebookLoginLink FacebookLoginUnsavedFormData)
            -> R.ReactElement
mkSocialFab facebookClientId mainColor darkColor icon hasValue mLink =
  Button.withStyles
    (\theme ->
      { root: createStyles
        { backgroundColor: mainColor
        , color: "#ffffff"
        , "&:hover": {backgroundColor: darkColor}
        }
      }
    )
    \{classes} ->
      button
        { variant: Button.fab
        , classes: Button.createClasses {root: classes.root}
        , disabled: case mLink of
          Nothing -> true
          _ -> false
        , href: case mLink of
          Nothing -> ""
          Just link -> URI.print (facebookLoginLinkToURI facebookClientId link)
        , style:
          if hasValue
            then createStyles
                  { backgroundColor: "#9df860"
                  }
            else createStyles {}
        } [icon]


type Effects eff =
  ( ref :: REF
  | eff)


mkSocialLogin :: forall eff siteLinks userDetails
               . ToLocation siteLinks
              => LocalCookingParams siteLinks userDetails (Effects eff)
              -> { env :: Env
                 , getUnsavedFormData :: Eff (Effects eff) FacebookLoginUnsavedFormData
                 }
              -> SocialLoginForm -> Array R.ReactElement
mkSocialLogin params {env,getUnsavedFormData} (SocialLoginForm {fb}) =
  [ mkSocialFab env.facebookClientId "#3b5998" "#1e3f82" facebookIcon (isJust fb) $
      Just $ FacebookLoginLink
      { redirectURL: params.toURI (toLocation FacebookLoginReturn)
      , state: FacebookLoginState
        { origin: toLocation $ unsafePerformEff $ IxSignal.get params.currentPageSignal
        , formData: Just (unsafePerformEff getUnsavedFormData)
        }
      }
  , mkSocialFab env.facebookClientId "#1da1f3" "#0f8cdb" twitterIcon false Nothing
  , mkSocialFab env.facebookClientId "#dd4e40" "#c13627" googleIcon false Nothing
  ]
