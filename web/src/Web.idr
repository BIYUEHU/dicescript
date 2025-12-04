module Web

import Web.Dom
import Web.Isx
import Data.List

%default total

public export
record ExampleBlock where
  constructor MkExample
  title : String
  defaultExpr : String
  examples : List String

renderBlock : ExampleBlock -> HTML
renderBlock ex =
  div [("class" .= "expr-block")] [
    h3 [] [text ex.title],
    div [("class" .= "expr-input-group")] [
      input [
        ("type" .= "text"),
        ("class" .= "expr-input"),
        ("placeholder" .= "Enter expression"),
        ("value" .= ex.defaultExpr)
      ],
      button [("class" .= "btn btn-run")] [text "Run"],
      button [("class" .= "btn btn-random")] [text "Random"]
    ],
    div [("class" .= "result empty")] [text "Result will appear here..."],
    div [("class" .= "examples")] [
      h4 [] [text "Examples:"],
      div [] (map (\e => code [] [text e]) ex.examples)
    ]
  ]

renderPage : List ExampleBlock -> HTML
renderPage blocks =
  div [("class" .= "container")] [
    section [("class" .= "intro")] [
      h1 [] [text "🎲 Dice Expression Playground"],
      p [] [text "A simple interpreter for dice notation expressions."],
      p [] [text "Try rolling some dice using standard RPG notation."]
    ],
    div [] (map renderBlock blocks)
  ]

exampleBlocks : List ExampleBlock
exampleBlocks = [
  MkExample
    "Basic Roll"
    "1d20"
    ["1d20", "2d6", "3d8+5"],
  MkExample
    "Arithmetic Operations"
    "2d6+1d4"
    ["2d6+1d4", "3d10-5", "2d8*2"],
  MkExample
    "Complex Expressions"
    "(1d6+2)*3"
    ["(1d6+2)*3", "4d6kh3", "2d20kl1"],
  MkExample
    "Advanced Features"
    "10d6>3"
    ["10d6>3", "5d10r1", "8d6!6"]
]

-- ============= 动态生成 DOM =============

-- 将 HTML DSL 转换并插入到 DOM
export
partial
renderToDOM : HasIO IO => Ptr Element -> HTML -> IO ()
renderToDOM container html = do
  clearInner container
  elem <- buildElement html
  appendChild container elem
  where
    buildElement : HTML -> IO $ Ptr Element
    buildElement (Text s) = createTextNode s
    buildElement (Element tag attrs children) = do
      elem <- createElement tag
      traverse_ (\(MkAttr name val) => setAttribute elem name val) attrs
      traverse_ (\child => do
          childElem <- buildElement child
          appendChild elem childElem
        ) children
      pure elem

-- 主入口：动态生成所有块
export
partial
main : IO ()
main = do
  Just container <- querySelector ".idris-dice-script-container"
    | Nothing => putStrLn "Container not found"

  -- 方案1: 使用 innerHTML (简单但可能有 XSS 风险)
  -- let htmlStr = renderHTML (renderPage exampleBlocks)
  -- setInnerHTML container htmlStr

  -- 方案2: 完全动态构建 DOM (安全)
  renderToDOM container (renderPage exampleBlocks)

  putStrLn "Demo blocks generated!"

  -- TODO: 绑定事件处理器
  -- setupEventHandlers container

-- ============= 事件处理（占位） =============

export
setupEventHandlers : HasIO IO => Ptr Element -> IO ()
setupEventHandlers container = do
  -- TODO:
  -- 1. 找到所有 .btn-run 按钮，绑定 runExpr
  -- 2. 找到所有 .btn-random 按钮，绑定 randomExpr
  -- 3. 找到所有 .examples code，绑定 loadExample
  pure ()

-- 实际的表达式运行逻辑
export
runExpr : String -> IO String
runExpr expr = do
  -- TODO: 调用你的 dice parser
  pure $ "Result of: " ++ expr

export
randomExpr : IO String
randomExpr = do
  -- TODO: 生成随机表达式
  pure "1d20+5"
