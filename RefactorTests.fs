module Fantomas.Refactor

open NUnit.Framework
open FsUnit
open Fantomas
open Refactor

let private prependNewline input =
    sprintf "\n%s" input |> String.normalizeNewLine

[<Test>]
let ``1 should become 2`` () =
    async {
        let! updatedCode =
            refactor
                changeOneToTwo
                """
                        let a = 1
                        let b = 2
                        """

        updatedCode
        |> prependNewline
        |> should
            equal
            (String.normalizeNewLine
                """
let a = 2
let b = 2
""")
    }
