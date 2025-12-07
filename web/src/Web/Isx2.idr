module Web.Isx2

import Web.Dom
import Data.String
import Data.List

%default total

public export
record Attr where
  constructor MkAttr
  name : String
  value : String

infixr 5 .=

export
(.=) : String -> String -> Attr
name .= value = MkAttr name value


public export
data ISX : Type where
  Text : String -> ISX
  Element : (tag : String) -> (attrs : List Attr) -> (children : List ISX) -> ISX

public export
interface Renderable a where
  render : a -> ISX

public export
implementation Renderable ISX where
  render = id

public export
implementation Renderable String where
  render = Text

public export
defineTag : Renderable a => (tag : String) -> (attrs : List Attr) -> (children : List a) -> ISX
defineTag tag attrs children = Element tag attrs (map render children)

export
div : Renderable a => List Attr -> List a -> ISX
div = defineTag "div"

export
section : Renderable a => List Attr -> List a -> ISX
section = defineTag "section"

export
h1 : Renderable a => List Attr -> List a -> ISX
h1 = defineTag "h1"

export
h3 : Renderable a => List Attr -> List a -> ISX
h3 = defineTag "h3"

export
h4 : Renderable a => List Attr -> List a -> ISX
h4 = defineTag "h4"

export
p : Renderable a => List Attr -> List a -> ISX
p = defineTag "p"

export
a : Renderable a => List Attr -> List a -> ISX
a = defineTag "a"

export
ul : Renderable a => List Attr -> List a -> ISX
ul = defineTag "ul"

export
li : Renderable a => List Attr -> List a -> ISX
li = defineTag "li"

export
input : List Attr -> ISX
input attrs = Element "input" attrs []

export
button : Renderable a => List Attr -> List a -> ISX
button = defineTag "button"

export
code : Renderable a => List Attr -> List a -> ISX
code = defineTag "code"

export
text : String -> ISX
text = Text

escapeHtml : String -> String
escapeHtml = concatMap escapeChar . unpack
  where
    escapeChar : Char -> String
    escapeChar c = case c of
      '<'  => "&lt;"
      '>'  => "&gt;"
      '&'  => "&amp;"
      '"'  => "&quot;"
      '\'' => "&#39;"
      _    => singleton c

renderAttr : Attr -> String
renderAttr (MkAttr name val) = name ++ "=\"" ++ escapeHtml val ++ "\""

renderAttrs : List Attr -> String
renderAttrs [] = ""
renderAttrs attrs = " " ++ unwords (map renderAttr attrs)

mutual
  export
  partial
  renderISX : Renderable a => a -> String
  renderISX a = case render a of
    Text str => escapeHtml str
    Element tag attrs children =>
      let attrsStr = renderAttrs attrs
          childrenStr = renderChildren children
      in if null children && isSelfClosing tag
        then "<" ++ tag ++ attrsStr ++ " />"
        else "<" ++ tag ++ attrsStr ++ ">" ++ childrenStr ++ "</" ++ tag ++ ">"

  partial
  renderChildren : Renderable a =>  List a -> String
  renderChildren = concat . map renderISX

  isSelfClosing : String -> Bool
  isSelfClosing "input" = True
  isSelfClosing "br" = True
  isSelfClosing "img" = True
  isSelfClosing _ = False


export
partial
renderToDOM : Renderable a => Element -> a -> IO ()
renderToDOM container html = do
  clearInner container
  elem <- buildElement $ render html
  appendChild container elem
  where
    buildElement : ISX -> IO $ Element
    buildElement (Text s) = createTextNode s
    buildElement (Element tag attrs children) = do
      elem <- createElement tag
      traverse_ (\(MkAttr name val) => setAttribute elem name val) attrs
      traverse_ (\child => do
          childElem <- buildElement child
          appendChild elem childElem
        ) children
      pure elem
