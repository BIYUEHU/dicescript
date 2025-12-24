export declare type IO<T> = () => T
export declare type DiceValue = boolean | number | DiceValue[] | ((x: unknown) => unknown)

declare const exportsObj: {
  runDice: (code: string) => IO<DiceValue>
  privEvaluate: ($0: unknown) => unknown
  privParse: ($0: unknown) => unknown
}

export default exportsObj
