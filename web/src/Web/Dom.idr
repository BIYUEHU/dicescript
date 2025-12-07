module Web.Dom

import Data.String
import Web.Es

%default total

export
data Element : Type where [external]

export
data Document : Type where [external]

%foreign """
javascript:lambda: (sel, just, nothing) => {
  const elem = document.querySelector(sel);
  return elem ? just(elem) : nothing;
}
"""
prim__querySelector : String -> (Element -> Maybe Element) -> Maybe Element -> PrimIO $ Maybe Element

export
querySelector : HasIO IO => String -> IO $ Maybe Element
querySelector sel = primIO $ prim__querySelector sel Just Nothing

export
%foreign "javascript:lambda: (sel) => document.querySelectorAll(sel)"
prim__querySelectorAll : String -> PrimIO $ EsIterator Element

export
querySelectorAll : HasIO IO => String -> IO $ EsIterator Element
querySelectorAll sel = primIO $ prim__querySelectorAll sel

%foreign "javascript:lambda: (tag) => document.createElement(tag)"
prim__createElement : String -> PrimIO Element

export
createElement : String -> IO Element
createElement tag = primIO $ prim__createElement tag

%foreign "javascript:lambda: (text) => document.createTextNode(text)"
prim__createTextNode : String -> PrimIO Element

export
createTextNode : String -> IO Element
createTextNode text = primIO $ prim__createTextNode text

%foreign "javascript:lambda: (parent, child) => parent.appendChild(child)"
prim__appendChild : Element -> Element -> PrimIO ()

export
appendChild : Element -> Element -> IO ()
appendChild parent child = primIO $ prim__appendChild parent child

%foreign """
javascript:lambda: (elem, attr, just, nothing) => {
  const val = elem.getAttribute(attr);
  return val ? just(val) : nothing;
}
"""
prim__getAttribute : Element -> String -> (String -> Maybe String) -> Maybe String -> PrimIO (Maybe String)

export
getAttribute : Element -> String -> IO (Maybe String)
getAttribute elem attr = primIO $ prim__getAttribute elem attr Just Nothing

%foreign "javascript:lambda: (elem, attr, val) => elem.setAttribute(attr, val)"
prim__setAttribute : Element -> String -> String -> PrimIO ()

export
setAttribute : Element -> String -> String -> IO ()
setAttribute elem attr val = primIO $ prim__setAttribute elem attr val

%foreign "javascript:lambda: (elem, just, nothing) => elem.textContent ? just(elem.textContent) : nothing"
prim__getTextContent : Element -> (String -> Maybe String) -> Maybe String -> PrimIO (Maybe String)

%foreign "javascript:lambda: (elem, prop) => { elem.textContent = prop }"
prim__setTextContent : Element -> String -> PrimIO ()

export
setTextContent : Element -> String -> IO ()
setTextContent elem text = primIO $ prim__setTextContent elem text

%foreign "javascript:lambda: (elem, just, nothing) => elem.value ? just(elem.value) : nothing"
prim__getElemValue : Element -> (String -> Maybe String) -> Maybe String -> PrimIO (Maybe String)

export
getElemValue : Element -> IO (Maybe String)
getElemValue elem = primIO $ prim__getElemValue elem Just Nothing

%foreign "javascript:lambda: (elem, val) => { elem.value = val }"
prim__setElemValue : Element -> String -> PrimIO ()

export
setElemValue : Element -> String -> IO ()
setElemValue elem val = primIO $ prim__setElemValue elem val

%foreign "javascript:lambda: (elem, just, nothing) => elem.innerHTML ? just(elem.innerHTML) : nothing"
prim__getInnerHTML : Element -> (String -> Maybe String) -> Maybe String -> PrimIO (Maybe String)

export
getInnerHTML : Element -> IO (Maybe String)
getInnerHTML elem = primIO $ prim__getInnerHTML elem Just Nothing

%foreign "javascript:lambda: (elem, html) => { elem.innerHTML = html }"
prim__setInnerHTML : Element -> String -> PrimIO ()

export
setInnerHTML : Element -> String -> IO ()
setInnerHTML elem html = primIO $ prim__setInnerHTML elem html

export
clearInner : Element -> IO ()
clearInner = flip setInnerHTML ""

%foreign "javascript:lambda: (elem, cls) => { elem.className = cls }"
prim__setClassName : Element -> String -> PrimIO ()

export
setClassName : Element -> String -> IO ()
setClassName elem cls = primIO $ prim__setClassName elem cls

%foreign "javascript:lambda:(elem, event, handler) => elem.addEventListener(event, handler)"
prim__addEventListener : Element -> String -> IO () -> PrimIO ()

export
addEventListener : Element -> String -> IO () -> IO ()
addEventListener elem event handler = primIO $ prim__addEventListener elem event handler
