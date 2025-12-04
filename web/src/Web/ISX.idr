module Web.Isx

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
data HTML : Type where
  Text : String -> HTML
  Element : (tag : String) -> (attrs : List Attr) -> (children : List HTML) -> HTML

export
div : List Attr -> List HTML -> HTML
div = Element "div"

export
section : List Attr -> List HTML -> HTML
section = Element "section"

export
h1 : List Attr -> List HTML -> HTML
h1 = Element "h1"

export
h3 : List Attr -> List HTML -> HTML
h3 = Element "h3"

export
h4 : List Attr -> List HTML -> HTML
h4 = Element "h4"

export
p : List Attr -> List HTML -> HTML
p = Element "p"

export
input : List Attr -> HTML
input attrs = Element "input" attrs []

export
button : List Attr -> List HTML -> HTML
button = Element "button"

export
code : List Attr -> List HTML -> HTML
code = Element "code"

export
text : String -> HTML
text = Text

-- ============= 渲染成 HTML String =============

escapeHtml : String -> String
escapeHtml s =
  ?s -- TODO: 实际应该转义 <, >, &, ", ' 等，这里简化了

renderAttr : Attr -> String
renderAttr (MkAttr name val) = name ++ "=\"" ++ escapeHtml val ++ "\""

renderAttrs : List Attr -> String
renderAttrs [] = ""
renderAttrs attrs = " " ++ unwords (map renderAttr attrs)

mutual
  export
  partial
  renderHTML : HTML -> String
  renderHTML (Text s) = escapeHtml s
  renderHTML (Element tag attrs children) =
    let attrsStr = renderAttrs attrs
        childrenStr = renderChildren children
    in if null children && isSelfClosing tag
      then "<" ++ tag ++ attrsStr ++ " />"
      else "<" ++ tag ++ attrsStr ++ ">" ++ childrenStr ++ "</" ++ tag ++ ">"

  partial
  renderChildren : List HTML -> String
  renderChildren = concat . map renderHTML

  isSelfClosing : String -> Bool
  isSelfClosing "input" = True
  isSelfClosing "br" = True
  isSelfClosing "img" = True
  isSelfClosing _ = False
