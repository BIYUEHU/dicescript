module Web.Dom

import Data.String

%default total

export
data Element : Type where [external]

export
data Document : Type where [external]

%foreign "javascript:lambda: (sel) => document.querySelector(sel)"
prim__querySelector : String -> PrimIO $ Ptr Element

%foreign "javascript:lambda: (tag) => document.createElement(tag)"
prim__createElement : String -> PrimIO $ Ptr Element

%foreign "javascript:lambda: (text) => document.createTextNode(text)"
prim__createTextNode : String -> PrimIO $ Ptr Element

%foreign "javascript:lambda: (parent, child) => parent.appendChild(child)"
prim__appendChild : Ptr Element -> Ptr Element -> PrimIO ()

%foreign "javascript:lambda: (elem, attr, val) => elem.setAttribute(attr, val)"
prim__setAttribute : Ptr Element -> String -> String -> PrimIO ()

%foreign "javascript:lambda: (elem, prop) => elem.textContent = prop"
prim__setTextContent : Ptr Element -> String -> PrimIO ()

%foreign "javascript:lambda: (elem) => elem.innerHTML = ''"
prim__clearInner : Ptr Element -> PrimIO ()

%foreign "javascript:lambda: (elem, cls) => elem.className = cls"
prim__setClassName : Ptr Element -> String -> PrimIO ()

%foreign "javascript:lambda: (v) => v == null || v == undefined"
prim_nullable : a -> Int

-- TODO: 这里说不定有问题 对于 IO
%foreign "javascript:lambda: (elem, event, handler) => elem.addEventListener(event, handler)"
prim__addEventListener : Ptr Element -> String -> IO () -> PrimIO ()

export
querySelector : HasIO IO => String -> IO $ Maybe (Ptr Element)
querySelector sel = do
  ptr <- primIO $ prim__querySelector sel
  pure $ if prim_nullable ptr == 1 then Nothing else Just ptr

export
createElement : HasIO IO => String -> IO $ Ptr Element
createElement tag = primIO $ prim__createElement tag

export
createTextNode : HasIO IO => String -> IO $ Ptr Element
createTextNode text = primIO $ prim__createTextNode text

export
appendChild : HasIO IO => Ptr Element -> Ptr Element -> IO ()
appendChild parent child = primIO $ prim__appendChild parent child

export
setAttribute : HasIO IO => Ptr Element -> String -> String -> IO ()
setAttribute elem attr val = primIO $ prim__setAttribute elem attr val

export
setTextContent : HasIO IO => Ptr Element -> String -> IO ()
setTextContent elem text = primIO $ prim__setTextContent elem text

export
clearInner : HasIO IO => Ptr Element -> IO ()
clearInner elem = primIO $ prim__clearInner elem

export
setClassName : HasIO IO => Ptr Element -> String -> IO ()
setClassName elem cls = primIO $ prim__setClassName elem cls

export
addEventListener : HasIO IO => Ptr Element -> String -> IO () -> IO ()
addEventListener elem event handler = primIO $ prim__addEventListener elem event handler
