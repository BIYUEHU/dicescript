module Exports

public export
%foreign """
javascript:lambda:(_, name, value) => () => {globalThis[name] = value;}
"""
exportGlobalJs : forall a. String -> a -> IO ()

public export
%foreign """
javascript:lambda:(_, name) => () => {delete globalThis[name];}
"""
cancelExportGlobalJs : String -> IO ()

public export
%foreign """
javascript:lambda:(_, name, value) => () => {module.exports[name] = value;}
"""
exportCommonJs : forall a. String -> a -> IO ()

public export
%foreign """
javascript:lambda:(_, name) => () => {delete module.exports[name];}
"""
cancelExportCommonJs : String -> IO ()

%foreign """
javascript:lambda:(t, f) => () => typeof process !== 'undefined' ? t : f
"""
prim_isNodeEnvironment : Bool -> Bool -> IO Bool

public export
isNodeEnvironment : IO Bool
isNodeEnvironment = prim_isNodeEnvironment True False
