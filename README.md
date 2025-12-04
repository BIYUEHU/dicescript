<!-- markdownlint-disable -->

<div align="center">

# DiceScript

<img src="public/logo.png" width="200px" height="200px" alt="logo"/>

**A safe and fast dice expression evaluator base on Idris2.**

</div>

<!-- markdownlint-enable -->

---

## What is DiceScript?

DiceScript is a superset of Mathematical (Arithmetic) Expression, it supports many advanced features, such as:

- Lambdas (Anonymous functions): `\x -> x+1` (Refers to Haskell lambda syntax)
- Array types: `[1, 2, 3]`
- Infix operaters: Dice `d`, Range `..`, Random Integer `~`, Concatenation `++`, Colon `:`
- More math functions: `round`, `floor`, `ceil`, `abs`, `sqrt`...
- Array functions: `map`, `filter`, `fold`, `reduce`, `flatMap`, `every`, `some`, `length`, `reverse`, `sort`, `include` ...
- Set functions: `union`, `intersection`, `difference` ...
- Random functions: `int`, `real`, `bool`, `shuffle`, `pick` ...
- List comprehension (Similar to Mathematical Set): `[x*2 | x <- [1..5]]`
- Math

> DiceScript is not a programming language, it is a DSL (Domain Specific Language) of base expression, an legal programm of DiceScript has only a expression.

## What is Idris2?

Idris2 is a dependently typed and functional programming language, it powerfully ensures type and program safety.

## Guide

Please refer to the [DiceScript Guide](GUIDE.md) for more details.

## Development

### Requirements

- Node.js
- Idris2
- Python

### Scripts

- `start`: Start the building process
- `dev`: Start the development watch mode
- `build`: Build the project
- `test`: Run the test cases

## License

Under the **BAN-ZHINESE-USING** License.
