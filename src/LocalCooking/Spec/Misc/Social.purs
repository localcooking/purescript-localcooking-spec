module LocalCooking.Spec.Misc.Social where

import Facebook.Types (FacebookClientId)
import Facebook.Call (FacebookLoginLink (..), facebookLoginLinkToURI)

import Prelude
import Data.Maybe (Maybe (..))
import Data.URI.URI (print) as URI
import React as R
import MaterialUI.Types (createStyles)
import MaterialUI.Button (button)
import MaterialUI.Button as Button




-- | For social logins
mkSocialFab :: FacebookClientId -> String -> String -> R.ReactElement
            -> Maybe FacebookLoginLink -> R.ReactElement
mkSocialFab facebookClientId mainColor darkColor icon mLink =
  Button.withStyles
    (\theme ->
      { root: createStyles
        { backgroundColor: mainColor
        , color: "#ffffff"
        , "&:hover": {backgroundColor: darkColor}
        }
      }
    )
    (\{classes} ->
      button
        { variant: Button.fab
        , classes: Button.createClasses {root: classes.root}
        , disabled: case mLink of
          Nothing -> true
          _ -> false
        , href: case mLink of
          Nothing -> ""
          Just link -> URI.print $
            facebookLoginLinkToURI facebookClientId link
        } [icon]
    )
