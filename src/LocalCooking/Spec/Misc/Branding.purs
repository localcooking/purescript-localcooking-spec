module LocalCooking.Spec.Misc.Branding where


import React as R
import React.DOM.SVG as R
import React.DOM.Props as RP


mainBrand :: R.ReactElement
mainBrand = R.path
  [ RP.d dVal
  , RP.unsafeMkProps "transform" "translate(-57.644531,-31.830094)"
  ]
  []

mainBrandViewBox :: String
mainBrandViewBox = "0 0 279 279"


dVal :: String
dVal = "m 88.080078,31.830078 c -16.861559,0 -30.435547,13.573988 -30.435547,30.435547 l 0,218.124995 c 0,16.86156 13.573988,30.43555 30.435547,30.43555 l 117.777342,0 c -28.05144,-10.58504 -47.40187,-34.05366 -52.04883,-62.03906 -3.32157,-20.00356 -0.0121,-39.86502 13.70118,-59.83594 l -43.05079,0.082 -0.0312,-157.203122 z m 69.205082,0 -0.10352,123.533202 65.55469,-0.0195 c 28.37978,0.2505 42.38874,1.01965 68.14844,23.44141 l -22.70508,22.18554 c -22.65671,-23.73197 -57.67481,-12.60291 -70.74024,4.91016 -18.38883,24.64863 -7.12077,54.49612 8.30664,66.92383 25.41516,20.47344 57.7876,5.32278 63.08203,-3.27735 l 21.80079,22.01563 c -6.52703,7.07483 -18.68801,14.05203 -30.09766,19.2832 l 45.67383,0 c 16.86156,0 30.43554,-13.57399 30.43554,-30.43555 l 0,-218.124995 c 0,-16.861559 -13.57398,-30.435547 -30.43554,-30.435547 z"
