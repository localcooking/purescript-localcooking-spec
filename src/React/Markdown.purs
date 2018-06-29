module React.Markdown where

import Prelude (Unit)
import React (ReactClass, ReactElement, createElement)
import Data.Record.Class (class Subrow)
import Data.String.Markdown (MarkdownText)



foreign import markdownImpl :: forall props. ReactClass props

type MarkdownProps o =
  { source :: MarkdownText
  | o
  }

type MarkdownPropsO renderers =
  ( className :: String
  , renderers :: { | renderers }
  )

type MarkdownRenderers =
  ( root :: ReactClass Unit
  , text :: String -> ReactElement
  , break :: ReactClass Unit
  , paragraph :: ReactClass Unit
  , emphasis :: ReactClass Unit
  , strong :: ReactClass Unit
  , thematicBreak :: ReactClass Unit
  , blockquote :: ReactClass Unit
  , delete :: ReactClass Unit
  , link :: ReactClass Unit
  , image :: ReactClass Unit
  , linkReference :: ReactClass Unit
  , imageReference :: ReactClass Unit
  , table :: ReactClass Unit
  , tableHead :: ReactClass Unit
  , tableBody :: ReactClass Unit
  , tableRow :: ReactClass Unit
  , tableCell :: ReactClass Unit
  , list :: ReactClass Unit
  , listItem :: ReactClass Unit
  , definition :: ReactClass Unit
  , heading :: ReactClass Unit
  , inlineCode :: ReactClass Unit
  , code :: ReactClass Unit
  , html :: ReactClass Unit
  )



markdown :: forall o renderers
          . Subrow o (MarkdownPropsO renderers)
         => Subrow renderers MarkdownRenderers
         => MarkdownProps o -> ReactElement
markdown props = createElement markdownImpl props []
