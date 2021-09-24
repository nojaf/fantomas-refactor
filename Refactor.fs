module Refactor

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Fantomas

let checker = FSharpChecker.Create()

let private getAST isFsi content =
    async {
        let fileName = if isFsi then "tmp.fsi" else "tmp.fsx"
        let sourceText = SourceOrigin.SourceString content
        let parsingOptions =
            { FSharpParsingOptions.Default with
                  SourceFiles = [| fileName |] }
        return! CodeFormatter.ParseAsync(fileName, sourceText, parsingOptions, checker)
    }

let zeroRange = Range.range0

let changeOneToTwo (tree: ParsedInput) : ParsedInput =
    let updateExpr (synExpr: SynExpr) : SynExpr =
        match synExpr with
        | SynExpr.Const (SynConst.Int32 1, _) -> SynExpr.Const(SynConst.Int32(2), zeroRange)
        | _ -> synExpr

    let updateBinding
        (SynBinding (accessibility,
                     kind,
                     mustInline,
                     isMutable,
                     attributes,
                     xmlDoc,
                     valData,
                     headPat,
                     returnInfo,
                     expr,
                     _,
                     seqPoint))
        : SynBinding =
        SynBinding.SynBinding(
            accessibility,
            kind,
            mustInline,
            isMutable,
            attributes,
            xmlDoc,
            valData,
            headPat,
            returnInfo,
            updateExpr expr,
            zeroRange,
            seqPoint
        )

    let updateDecl (decl: SynModuleDecl) : SynModuleDecl =
        match decl with
        | SynModuleDecl.Let (isRecursive, bindings, _) ->
            SynModuleDecl.Let(isRecursive, List.map updateBinding bindings, zeroRange)
        | _ -> decl

    let updateModule
        (SynModuleOrNamespace (longId, isRecursive, kind, decls, xmlDoc, attribs, accessibility, _))
        : SynModuleOrNamespace =
        SynModuleOrNamespace(
            longId,
            isRecursive,
            kind,
            List.map updateDecl decls,
            xmlDoc,
            attribs,
            accessibility,
            zeroRange
        )

    match tree with
    | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput (fileName,
                                                                     isScript,
                                                                     qualifiedNameOfFile,
                                                                     scopedPragma,
                                                                     hashDirectives,
                                                                     modules,
                                                                     isLastCompiland)) ->
        let nextModules = List.map updateModule modules

        ParsedInput.ImplFile(
            ParsedImplFileInput.ParsedImplFileInput(
                fileName,
                isScript,
                qualifiedNameOfFile,
                scopedPragma,
                hashDirectives,
                nextModules,
                isLastCompiland
            )
        )
    | _ -> tree

let refactor (updateTree: ParsedInput -> ParsedInput) (sourceCode: string) : Async<string> =
    async {
        let! originalAST = getAST false sourceCode

        let updatedTree =
            updateTree (originalAST |> Array.head |> fst)

        return! CodeFormatter.FormatASTAsync(updatedTree, "tmp.fsx", [], None, FormatConfig.FormatConfig.Default)
    }
