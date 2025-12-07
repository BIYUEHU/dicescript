module Web.Es


export
data EsIterator : Type -> Type where [external]

%foreign "javascript:lambda: (_, iterator, callback) => { for (const item of iterator) callback(item)(); }"
prim__for_ : EsIterator a -> (a -> IO ()) -> PrimIO ()

export
for_ : EsIterator a -> (a -> IO ()) -> IO ()
for_ it callback = primIO $ prim__for_ it callback

export
data EsArray : Type -> Type where [external]

%foreign "javascript:lambda: array => array.length"
prim__length : EsArray a -> Int

export
length : EsArray a -> Int
length arr = prim__length arr

%foreign "javascript:lambda: (_, __, array, callback) => array.map((el) => callback(el))"
prim__map : EsArray a -> (a -> b) -> EsArray b

export
implementation Functor EsArray where
  map = flip prim__map


%foreign "javascript:lambda: (_, v) => v === null || v === undefined"
prim__nullable : a -> Int

export
nullable : a -> Bool
nullable v = prim__nullable v == 1

%foreign "javascript:lambda: (_, s) => console.log(s)"
prim__log : a -> PrimIO ()

export
log : String -> IO ()
log s = primIO $ prim__log s

export
debug : a -> IO ()
debug s = primIO $ prim__log s
