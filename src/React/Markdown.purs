module React.Markdown where

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

type MarkdownRenderers
  rootProps
  breakProps
  paragraphProps
  emphasisProps
  strongProps
  thematicBreakProps
  blockquoteProps
  deleteProps
  linkProps
  imageProps
  linkReferenceProps
  imageReferenceProps
  tableProps
  tableHeadProps
  tableBodyProps
  tableRowProps
  tableCellProps
  listProps
  listItemProps
  definitionProps
  headingProps
  inlineCodeProps
  codeProps
  htmlProps
  =
  ( root :: ReactClass rootProps
  , text :: String -> ReactElement
  , break :: ReactClass breakProps
  , paragraph :: ReactClass paragraphProps
  , emphasis :: ReactClass emphasisProps
  , strong :: ReactClass strongProps
  , thematicBreak :: ReactClass thematicBreakProps
  , blockquote :: ReactClass blockquoteProps
  , delete :: ReactClass deleteProps
  , link :: ReactClass linkProps
  , image :: ReactClass imageProps
  , linkReference :: ReactClass linkReferenceProps
  , imageReference :: ReactClass imageReferenceProps
  , table :: ReactClass tableProps
  , tableHead :: ReactClass tableHeadProps
  , tableBody :: ReactClass tableBodyProps
  , tableRow :: ReactClass tableRowProps
  , tableCell :: ReactClass tableCellProps
  , list :: ReactClass listProps
  , listItem :: ReactClass listItemProps
  , definition :: ReactClass definitionProps
  , heading :: ReactClass headingProps
  , inlineCode :: ReactClass inlineCodeProps
  , code :: ReactClass codeProps
  , html :: ReactClass htmlProps
  )



markdown :: forall o renderers
            rootProps breakProps paragraphProps emphasisProps strongProps
            thematicBreakProps blockquoteProps deleteProps linkProps imageProps
            linkReferenceProps imageReferenceProps tableProps tableHeadProps
            tableBodyProps tableRowProps tableCellProps listProps listItemProps
            definitionProps headingProps inlineCodeProps codeProps htmlProps
          . Subrow o (MarkdownPropsO renderers)
         => Subrow renderers
            ( MarkdownRenderers
                rootProps
                breakProps
                paragraphProps
                emphasisProps
                strongProps
                thematicBreakProps
                blockquoteProps
                deleteProps
                linkProps
                imageProps
                linkReferenceProps
                imageReferenceProps
                tableProps
                tableHeadProps
                tableBodyProps
                tableRowProps
                tableCellProps
                listProps
                listItemProps
                definitionProps
                headingProps
                inlineCodeProps
                codeProps
                htmlProps
            )
         => MarkdownProps o -> ReactElement
markdown props = createElement markdownImpl props []
