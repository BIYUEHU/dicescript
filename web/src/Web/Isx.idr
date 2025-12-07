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
data ISX : Type where
  Text : String -> ISX
  Element : (tag : String) -> (attrs : List Attr) -> (children : List ISX) -> ISX

export
div : List Attr -> List ISX -> ISX
div = Element "div"

export
section : List Attr -> List ISX -> ISX
section = Element "section"

export
h1 : List Attr -> List ISX -> ISX
h1 = Element "h1"

export
h3 : List Attr -> List ISX -> ISX
h3 = Element "h3"

export
h4 : List Attr -> List ISX -> ISX
h4 = Element "h4"

export
p : List Attr -> List ISX -> ISX
p = Element "p"

export
input : List Attr -> ISX
input attrs = Element "input" attrs []

export
button : List Attr -> List ISX -> ISX
button = Element "button"

export
code : List Attr -> List ISX -> ISX
code = Element "code"

export
text : String -> ISX
text = Text

-- ============= 渲染成 ISX String =============

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
  renderISX : ISX -> String
  renderISX (Text s) = escapeHtml s
  renderISX (Element tag attrs children) =
    let attrsStr = renderAttrs attrs
        childrenStr = renderChildren children
    in if null children && isSelfClosing tag
      then "<" ++ tag ++ attrsStr ++ " />"
      else "<" ++ tag ++ attrsStr ++ ">" ++ childrenStr ++ "</" ++ tag ++ ">"

  partial
  renderChildren : List ISX -> String
  renderChildren = concat . map renderISX

  isSelfClosing : String -> Bool
  isSelfClosing "input" = True
  isSelfClosing "br" = True
  isSelfClosing "img" = True
  isSelfClosing _ = False
