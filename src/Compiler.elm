module Compiler exposing (..)

import Ast
import Code exposing (..)
import Ast.Statement exposing (Statement)
import List
import Helpers exposing (..)
import ExContext exposing (Context, Aliases)
import ExAlias
import ExStatement
import Dict
import Regex exposing (..)


version : String
version =
    "0.4.5"


glueStart : String
glueStart =
    (ind 0)
        ++ "use Elchemy"
        ++ "\n"


glueEnd : String
glueEnd =
    "\n"
        ++ String.trim
            """
         end
         """


getName : String -> ( String, String )
getName file =
    case String.split "\n" file of
        n :: rest ->
            ( n, String.join "\n" rest )

        [] ->
            ( "", "" )


tree : String -> String
tree m =
    case String.split ">>>>" m of
        [ single ] ->
            single
                |> parse "NoName.elm"
                |> getContext
                |> (\( c, a ) ->
                        case c of
                            Nothing ->
                                Debug.crash "Failed getting context"

                            Just c ->
                                (getCode c a).product
                   )

        multiple ->
            let
                files =
                    multiple
                        |> List.map getName
                        |> List.map (\( name, code ) -> ( name, parse name code ))

                wContexts =
                    files
                        |> List.map (\( name, ast ) -> ( name, getContext ast ))
                        |> List.filterMap
                            (\a ->
                                case a of
                                    ( _, ( Nothing, _ ) ) ->
                                        Nothing

                                    ( name, ( Just c, ast ) ) ->
                                        Just ( name, c, ast )
                            )

                commonAliases =
                    wContexts
                        |> List.map (\( name, ctx, ast ) -> ctx.aliases)
                        |> getCommonAliases

                wTrueContexts =
                    wContexts
                        |> List.map (\( name, c, ast ) -> ( name, { c | aliases = commonAliases }, ast ))
            in
                wTrueContexts
                    |> List.map
                        (\( name, c, ast ) ->
                            ">>>>" ++ name ++ "\n" ++ (getCode c ast).product
                        )
                    |> String.join "\n"


getCommonAliases : List Aliases -> Aliases
getCommonAliases a =
    List.foldl
        (\aliases acc ->
            Dict.merge
                Dict.insert
                typeAliasDuplicate
                Dict.insert
                acc
                aliases
                Dict.empty
        )
        (Dict.empty)
        a


typeAliasDuplicate : comparable -> a -> a -> Dict.Dict comparable a -> Dict.Dict comparable a
typeAliasDuplicate k v v2 =
    if v /= v2 then
        Debug.crash ("You can't have two different type aliases for " ++ toString k)
    else
        Dict.insert k v


getContext : List Statement -> ( Maybe Context, List Statement )
getContext statements =
    case statements of
        [] ->
            ( Nothing, [] )

        mod :: statements ->
            let
                base =
                    ExStatement.moduleStatement mod
            in
                ( Just (ExAlias.getAliases base statements), statements )


aggregateStatements : Statement -> Code -> Code
aggregateStatements s code =
    let
        ( newC, newCode ) =
            ExStatement.elixirS code s
    in
        code
            <||> (always newC)
            |+ newCode


getCode : Context -> List Statement -> Code
getCode context statements =
    let
        code =
            Code.empty context
    in
        code
            |+ "# Compiled using Elchemy v"
            |+ version
            |+ "\n"
            |+ ("defmodule " ++ context.mod ++ " do")
            |+ glueStart
            |> (\c -> List.foldl aggregateStatements c statements)
            |+ glueEnd


parse : String -> String -> List Statement
parse fileName m =
    case Ast.parse (prepare m) of
        Ok ( _, _, statements ) ->
            statements

        Err ( (), { input, position }, [ msg ] ) ->
            Debug.crash
                ("]ERR> Compilation error in:\n "
                    ++ fileName
                    ++ "\nat:\n "
                    ++ (input
                            |> String.lines
                            |> List.take 30
                            |> String.join "\n"
                       )
                    ++ "\n"
                )

        err ->
            Debug.crash (toString err)


prepare : String -> String
prepare codebase =
    codebase |> removeComments


removeComments : String -> String
removeComments =
    -- Need to remove the second one
    Regex.replace All (regex "\\s--.*\n") (always "")
        >> Regex.replace All (regex "\n +\\w+ : .*") (always "")
