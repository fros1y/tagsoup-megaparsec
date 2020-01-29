-- |

module Text.Megaparsec.Test where



import Data.Text ( Text )
import qualified Data.Text as T
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HMS
import Text.HTML.TagSoup
import Text.Megaparsec
import Text.Megaparsec.Error
import Text.Megaparsec.TagSoup
import Data.Void


type AttrName   = Text
type AttrValue  = Text

data Element = Element
  { elementName :: !Text
  , elementAttrs :: !(HashMap AttrName AttrValue)
  , elementChildren :: [Node]
  } deriving (Eq, Show)

data Node =
    ElementNode Element
  | TextNode Text
  deriving (Eq, Show)

type Parser = TagParser Text

node :: Parser Node
node = ElementNode <$> element
   <|> TextNode <$> text

text :: Parser Text
text =  fromTagText <$> tagText

element :: Parser Element
element = do
  t@(TagOpen tagName attrs) <- anyTagOpen
  children <- many node
  closeTag@(TagClose tagName') <- anyTagClose
  if tagName == tagName'
     then return $ Element tagName (HMS.fromList attrs) children
     else error "error"


--parseDOM :: Text -> Either (ParseError Text Node) [Node]
parseDOM html = parse (many node) "" tags
  where tags = parseTags html
