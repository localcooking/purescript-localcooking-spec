module LocalCooking.Spec.Misc.Network where

import LocalCooking.Spec.Misc.Branding (mainBrand)

import Prelude
import MaterialUI.Types (createStyles)
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.SvgIcon (svgIcon)
import MaterialUI.SvgIcon as SvgIcon

import React (ReactElement)
import React.DOM (text)



networkButton :: { dark :: String
                 , light :: String
                 , href :: String
                 , label :: String
                 }
              -> ReactElement
networkButton {dark,light,href,label} =
  Button.withStyles
  (\_ ->
    { root: createStyles
      { background: dark
      , color: "#fff"
      , textTransform: "none"
      , "&:hover":
        { background: light
        }
      }
    }
  )
  \{classes} ->
    button
    { href
    , classes: Button.createClasses classes
    , variant: Button.raised
    }
    [ svgIcon
      { viewBox: "0 0 279 279"
      , color: SvgIcon.inherit
      }
      [ mainBrand
      ]
    , text (" " <> label)
    ]
