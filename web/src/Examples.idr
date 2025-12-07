module Examples

import Web.Isx2

public export
record ExampleBlock where
  constructor MkExample
  title : String
  example : String

public export
implementation Renderable ExampleBlock where
  render (MkExample title example) =
    let id = show title in
    div [("class" .= "expr-block")] [
      h3 [] [title],
      div [("class" .= "expr-input-group")] [
        input [
          ("type" .= "text"),
          ("class" .= "expr-input"),
          ("placeholder" .= "Enter expression"),
          ("value" .= example),
          ("data-input-id" .= id)
        ],
        button [("class" .= "btn btn-run"), ("data-run-id" .= id)] ["Run"]
      ],
      div [("class" .= "result empty"), ("data-result-id" .= id)] ["Result will appear here..."]
    ]


export
exampleBlocks : List ExampleBlock
exampleBlocks = [
  -- Basic Dice Rolling
  MkExample "Single Roll"                    "1d20",
  MkExample "Multiple Dice"                  "4d6",
  MkExample "Zero Dice → Empty Array"        "0d8",

  -- Arithmetic & Grouping
  MkExample "Basic Arithmetic"               "2d6 + 3",
  MkExample "Complex Expression"             "(1d6 + 2) * 4 - 1",
  MkExample "Exponentiation"                 "2 ^ 10",
  MkExample "Modulo"                         "23 % 10",

  -- Range & Random Integer
  MkExample "Range Operator (..)"            "1 .. 6",
  MkExample "Descending Range"               "10 .. 5",
  MkExample "Random Integer (~)"             "1~100",

  -- Array Construction & Manipulation
  MkExample "Cons Operator"                  "1 : [2, 3]",
  MkExample "Concatenation"                  "[1, 2] ++ [3, 4]",
  MkExample "Literal Array"                  "[1, 2, 3, 4]",
  MkExample "Empty Array"                    "[]",

  -- Comparison Chains
  MkExample "Equality Chain"                 "5 == 5 == 5",
  MkExample "Inequality Chain"               "1 != 2 != 2",
  MkExample "Ordered Comparison"             "1 < 2 <= 3 > 2",

  -- Logical Operators
  MkExample "AND / OR"                       "True && False || True",
  MkExample "NOT"                            "!False",

  -- Dice with Filters (common pattern)
  MkExample "Keep Highest"                   "4d6>3",           -- roll 4d6, keep those >3
  MkExample "Exploding Dice (custom)"        "1d6++",           -- depends on your impl

  -- Lambda & Function Call
  MkExample "Lambda (add 1)"                 "(\\x -> x + 1)(5)",
  MkExample "Multi-arg Lambda"               "(\\x, y -> x * y)(3, 7)",

  -- Built-in Math Functions
  MkExample "Trig Functions"                 "sin(p/2)",
  MkExample "Log & Exp"                      "log(e)",
  MkExample "Rounding"                       "floor(3.7)",
  MkExample "Max / Min"                      "max(1, 5, 3)",

  -- Array Functions
  MkExample "Length"                         "length([1,2,3,4])",
  MkExample "Sum"                            "sum(4d6)",
  MkExample "Slice"                          "slice([1 .. 10], 2, 4)",
  MkExample "Shuffle"                        "shuffle([1..5])",
  MkExample "Pick Random Elements"           "pick([1 .. 100], 3)",

  -- Set Operations
  MkExample "Union"                          "union([1,2], [2,3])",
  MkExample "Intersection"                   "intersection([1,2,3], [2,3,4])",
  MkExample "Difference"                     "difference([1,2,3], [2])",
  MkExample "Contains All"                   "contain([1,2,3,4], [2,4])",

  -- Random Functions
  MkExample "Random Float"                   "real(0, 1)",
  MkExample "Random Bool (70%)"              "bool(0.7)",
  MkExample "Random Int (inclusive)"         "int(1, 7)",

  -- Conditional
  MkExample "If Expression"                  "if(1d6 > 3, 111111, 23333)",

  -- Constants
  MkExample "Pi"                             "p",
  MkExample "Eulers Number"                 "e"
]