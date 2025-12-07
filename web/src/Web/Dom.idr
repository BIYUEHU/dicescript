module Web.Dom

import Data.String
import Web.Es

%default total

export
data Element : Type where [external]

export
data Document : Type where [external]

%foreign "javascript:lambda: (sel) => document.querySelector(sel)"
prim__querySelector : String -> PrimIO Element

%foreign "javascript:lambda: (sel) => document.querySelectorAll(sel)"
prim__querySelectorAll : String -> PrimIO $ EsIterator Element

%foreign "javascript:lambda: (tag) => document.createElement(tag)"
prim__createElement : String -> PrimIO Element

%foreign "javascript:lambda: (text) => document.createTextNode(text)"
prim__createTextNode : String -> PrimIO Element

%foreign "javascript:lambda: (parent, child) => parent.appendChild(child)"
prim__appendChild : Element -> Element -> PrimIO ()

%foreign "javascript:lambda: (elem, attr) => elem.getAttribute(attr)"
prim__getAttribute : Element -> String -> PrimIO String

%foreign "javascript:lambda: (elem, attr, val) => elem.setAttribute(attr, val)"
prim__setAttribute : Element -> String -> String -> PrimIO ()

%foreign "javascript:lambda: (elem) => elem.textContent"
prim__getTextContent : Element -> PrimIO String

%foreign "javascript:lambda: (elem, prop) => elem.textContent = prop"
prim__setTextContent : Element -> String -> PrimIO ()

%foreign "javascript:lambda: (elem) => elem.value"
prim__getElmValue : Element -> PrimIO String

%foreign "javascript:lambda: (elem, val) => elem.value = val"
prim__setElmValue : Element -> String -> PrimIO ()

%foreign "javascript:lambda: (elem) => elem.innerHTML"
prim__getInnerHTML : Element -> PrimIO String

%foreign "javascript:lambda: (elem, html) => elem.innerHTML = html"
prim__setInnerHTML : Element -> String -> PrimIO ()

%foreign "javascript:lambda: (elem, cls) => elem.className = cls"
prim__setClassName : Element -> String -> PrimIO ()

-- TODO: 这里说不定有问题 对于 IO
%foreign "javascript:lambda:(elem, event, handler) => elem.addEventListener(event, handler)"
prim__addEventListener : Element -> String -> IO () -> PrimIO ()

export
querySelector : HasIO IO => String -> IO $ Maybe Element
querySelector sel = do
  ptr <- primIO $ prim__querySelector sel
  pure $ if nullable ptr then Nothing else Just ptr

export
querySelectorAll : HasIO IO => String -> IO $ EsIterator Element
querySelectorAll sel = primIO $ prim__querySelectorAll sel

export
createElement : HasIO IO => String -> IO Element
createElement tag = primIO $ prim__createElement tag

export
createTextNode : HasIO IO => String -> IO Element
createTextNode text = primIO $ prim__createTextNode text

export
appendChild : HasIO IO => Element -> Element -> IO ()
appendChild parent child = primIO $ prim__appendChild parent child

export
getAttribute : HasIO IO => Element -> String -> IO (Maybe String)
getAttribute elem attr = do
  val <- primIO $ prim__getAttribute elem attr
  pure $ if nullable val then Nothing else Just val

export
setAttribute : HasIO IO => Element -> String -> String -> IO ()
setAttribute elem attr val = primIO $ prim__setAttribute elem attr val

export
getTextContent : HasIO IO => Element -> IO (Maybe String)
getTextContent elem = do
  val <- primIO $ prim__getTextContent elem
  pure $ if nullable val then Nothing else Just val

export
setTextContent : HasIO IO => Element -> String -> IO ()
setTextContent elem text = primIO $ prim__setTextContent elem text

export
getElmValue : HasIO IO => Element -> IO (Maybe String)
getElmValue elem = do
  val <- primIO $ prim__getElmValue elem
  pure $ if nullable val then Nothing else Just val

export
setElmValue : HasIO IO => Element -> String -> IO ()
setElmValue elem val = primIO $ prim__setElmValue elem val

export
getInnerHTML : HasIO IO => Element -> IO (Maybe String)
getInnerHTML elem = do
  val <- primIO $ prim__getInnerHTML elem
  pure $ if nullable val then Nothing else Just val

export
setInnerHTML : HasIO IO => Element -> String -> IO ()
setInnerHTML elem html = primIO $ prim__setInnerHTML elem html

export
clearInner : HasIO IO => Element -> IO ()
clearInner = flip setInnerHTML ""

export
setClassName : HasIO IO => Element -> String -> IO ()
setClassName elem cls = primIO $ prim__setClassName elem cls

export
addEventListener : HasIO IO => Element -> String -> IO () -> IO ()
addEventListener elem event handler = primIO $ prim__addEventListener elem event handler
