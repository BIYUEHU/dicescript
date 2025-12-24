module JsValue

import Dice.Value
import Data.List

export
data JSValue : Type where [external]

%foreign "javascript:lambda:(x) => x"
prim__bool : Bool -> JSValue

%foreign "javascript:lambda:(x) => x"
prim__number : Double -> JSValue

%foreign """
javascript:lambda:(list) => {
  const result = [];
  let current = list;
  while (current && current.a1 !== undefined) {
    result.push(current.a1);
    current = current.a2;
  }
  return result;
}
"""
prim__array : List JSValue -> JSValue

%foreign "javascript:lambda:(params, bodyFn) => (...args) => bodyFn(...args)"
prim__function : List String -> (List (JSValue) -> JSValue) -> JSValue

mutual
  export
  valueToJS : Value -> JSValue
  valueToJS (VBool b) = prim__bool b
  valueToJS (VNum n) = prim__number n
  valueToJS (VArray xs) = prim__array (map valueToJS xs)
  valueToJS (VLambda params body) =
    prim__function params (\args => prim__number 0.0)


export
%foreign "javascript:lambda:(_, s) => { throw new Error(s) }"
throw : (returnType: Type) -> String -> returnType
