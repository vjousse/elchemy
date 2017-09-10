module ExStatement exposing (elixirS, moduleStatement)

import Ast
import ExFfi
import ExType
import ExFunction
import ExExpression
import Dict exposing (Dict)
import Ast.BinOp exposing (operators)
import Ast.Expression exposing (Expression(..))
import Regex exposing (Regex, HowMany(..), regex)
import Ast.Statement exposing (Statement(..), Type(..), ExportSet(..))
import ExContext exposing (Context, Definition, indent, deindent, onlyWithoutFlag)
import Helpers
    exposing
        ( modulePathName
        , ind
        , indAll
        , indNoNewline
        , prependAll
        , toSnakeCase
        , operatorType
        , isCustomOperator
        , Operator(..)
        , translateOperator
        , (=>)
        , modulePath
        , notImplemented
        , typeApplicationToList
        )


type ElchemyComment
    = Doc String
    | Ex String
    | Normal String
    | Flag String


type DocType
    = Fundoc
    | Typedoc
    | ModuleDoc


{-| Make sure first statement is a module declaration
-}
moduleStatement : Statement -> Context
moduleStatement s =
    case s of
        ModuleDeclaration path exports ->
            ExContext.empty (modulePathName path) exports

        other ->
            Debug.crash "First statement must be module declaration"


{-| Encode any statement
-}
elixirS : Context -> Statement -> ( Context, String )
elixirS c s =
    case s of
        InfixDeclaration _ _ _ ->
            ( c, "" )

        TypeDeclaration (TypeConstructor [ name ] _) types ->
            let
                ( newC, code ) =
                    c.lastDoc
                        |> Maybe.map (elixirDoc c Typedoc)
                        |> Maybe.withDefault ( c, "" )
            in
                (,) newC <|
                    code
                        ++ (ind c.indent)
                        ++ "@type "
                        ++ toSnakeCase True name
                        ++ " :: "
                        ++ (List.map (ExType.uniontype c) types |> String.join " | ")
                        ++ "\n"

        TypeAliasDeclaration _ _ ->
            ( c, "" )

        (FunctionTypeDeclaration name ((TypeApplication _ _) as t)) as def ->
            let
                definition =
                    getTypeDefinition def

                ( newC, code ) =
                    c.lastDoc
                        |> Maybe.map (elixirDoc c Fundoc)
                        |> Maybe.withDefault ( c, "" )
            in
                (,) (addTypeDefinition newC name definition) <|
                    (onlyWithoutFlag newC "nodef" name code)
                        ++ case operatorType name of
                            Builtin ->
                                -- TODO implement operator specs
                                ""

                            Custom ->
                                onlyWithoutFlag newC "nospec" name <|
                                    (ind newC.indent)
                                        ++ "@spec "
                                        ++ translateOperator name
                                        ++ (ExType.typespec newC t)

                            None ->
                                onlyWithoutFlag newC "nospec" name <|
                                    (ind newC.indent)
                                        ++ "@spec "
                                        ++ toSnakeCase True name
                                        ++ (ExType.typespec newC t)

        (FunctionTypeDeclaration name t) as def ->
            let
                definition =
                    getTypeDefinition def

<<<<<<< HEAD
        (FunctionDeclaration name args body) as fd ->
            (,) c <| handleFunctionDeclaration c name args body

        Comment content ->
            handleComment c content
=======
                ( newC, code ) =
                    c.lastDoc
                        |> Maybe.map (elixirDoc c Fundoc)
                        |> Maybe.withDefault ( c, "" )
            in
                (,) (addTypeDefinition newC name definition) <|
                    code
                        ++ case operatorType name of
                            Builtin ->
                                -- TODO implement operator specs
                                ""

                            Custom ->
                                onlyWithoutFlag newC name "nospec" <|
                                    (ind c.indent)
                                        ++ "@spec "
                                        ++ translateOperator name
                                        ++ (ExType.typespec newC t)

                            None ->
                                onlyWithoutFlag newC name "nospec" <|
                                    (ind c.indent)
                                        ++ "@spec "
                                        ++ toSnakeCase True name
                                        ++ (ExType.typespec newC t)

        (FunctionDeclaration name args body) as fd ->
            let
                genFfi =
                    ExFfi.generateFfi c ExExpression.elixirE name <|
                        (c.definitions
                            |> Dict.get name
                            |> Maybe.map
                                (.def
                                    >> typeApplicationToList
                                )
                            |> Maybe.withDefault []
                            |> List.map typeApplicationToList
                        )
            in
                c
                    => if
                        Dict.get name c.definitions
                            == Nothing
                            && not (ExContext.isPrivate c name)
                       then
                        Debug.crash <|
                            "To be able to export it, you need to provide function type for `"
                                ++ name
                                ++ "` function in module "
                                ++ toString c.mod
                       else
                        case body of
                            (Application (Application (Variable [ "ffi" ]) _) _) as app ->
                                genFfi app

                            (Application (Application (Variable [ "tryFfi" ]) _) _) as app ->
                                genFfi app

                            Case (Tuple vars) expressions ->
                                if vars == args then
                                    ExFunction.genOverloadedFunctionDefinition c ExExpression.elixirE name args body expressions
                                else
                                    ExFunction.genFunctionDefinition c ExExpression.elixirE name args body

                            _ ->
                                ExFunction.genFunctionDefinition c ExExpression.elixirE name args body

        Comment content ->
            elixirComment c content
>>>>>>> dev

        -- That's not a real import. In elixir it's called alias
        ImportStatement path Nothing Nothing ->
            c
                => (ind c.indent)
                ++ "alias "
                ++ modulePath path

        ImportStatement path (Just asName) Nothing ->
            c
                => (ind c.indent)
                ++ "alias "
                ++ modulePath path
                ++ ", as: "
                ++ asName

        ImportStatement path Nothing (Just ((SubsetExport exports) as subset)) ->
            ExContext.mergeTypes subset (modulePathName path) c
                => (ind c.indent)
                ++ "import "
                ++ modulePath path
                ++ ", only: ["
                ++ (List.map subsetExport exports |> List.foldr (++) [] |> String.join ",")
                ++ "]"

        -- Suppresses the compiler warning
        ImportStatement [ "Elchemy" ] Nothing (Just AllExport) ->
            ( c, "" )

        ImportStatement path Nothing (Just AllExport) ->
            ExContext.mergeTypes AllExport (modulePathName path) c
                => (ind c.indent)
                ++ "import "
                ++ modulePath path

        s ->
            (,) c <|
                notImplemented "statement" s


<<<<<<< HEAD
handleFunctionDeclaration : Context -> String -> List Expression -> Expression -> String
handleFunctionDeclaration c name args body =
    let
        returns t =
            c.definitions
                |> Dict.get name
                |> Maybe.map (.def >> ExType.hasReturnedType t)
                |> Maybe.withDefault False

        typeDefinition =
            (c.definitions
                |> Dict.get name
                |> Maybe.map
                    (.def
                        >> typeAplicationToList
                    )
                |> Maybe.withDefault []
                |> map typeAplicationToList
            )
    in
        if name == "meta" && args == [] then
            ExExpression.generateMeta body
        else
            case body of
                Access (Variable ("Native" :: rest)) [ call ] ->
                    ExExpression.generateFfi
                        c
                        name
                        typeDefinition
                        (Application
                            (Application (Variable [ "ffi" ])
                                (String (String.join "." rest))
                            )
                            (String call)
                        )

                (Application (Application (Variable [ "io" ]) _) _) as app ->
                    if returns (TypeConstructor [ "Cmd" ] ([ TypeVariable "a" ])) then
                        ExExpression.generateFfi
                            c
                            name
                            typeDefinition
                            app
                    else
                        Debug.crash "io has to return Cmd a"

                (Application (Application (Variable [ "ffi" ]) _) _) as app ->
                    ExExpression.generateFfi
                        c
                        name
                        typeDefinition
                        app

                (Application (Application (Variable [ "tryFfi" ]) _) _) as app ->
                    if returns (TypeConstructor [ "Result" ] ([ TypeConstructor [ "String" ] [], TypeVariable "a" ])) then
                        ExExpression.generateFfi
                            c
                            name
                            typeDefinition
                            app
                    else
                        Debug.crash "io has to return `Result String a`"

                Case vars expressions ->
                    if ExExpression.flattenCommas vars == args then
                        ExExpression.genOverloadedFunctionDefinition
                            c
                            name
                            args
                            body
                            expressions
                    else
                        ExExpression.genFunctionDefinition
                            c
                            name
                            args
                            body

                _ ->
                    ExExpression.genFunctionDefinition
                        c
                        name
                        args
                        body


handleComment : Context -> String -> ( Context, String )
handleComment c content =
    case getCommentType content of
        Doc content ->
            (,) c <|
                (ind c.indent)
                    ++ "@doc \"\"\"\n "
                    ++ (content
                            |> String.lines
                            |> map (maybeDoctest c)
                            |> map (Helpers.escape)
                            |> map (flip (++) (ind c.indent))
                            |> map trimIndentations
                            |> String.join ""
                            -- Drop an unnecessary \n at the end
                            |> String.dropRight 1
                       )
                    ++ (ind c.indent)
                    ++ "\"\"\""

        Ex content ->
            (,) c <|
                (content
                    |> String.split "\n"
                    |> map String.trim
                    |> String.join "\n"
                    |> indAll c.indent
                )

        Flag content ->
            flip (,) "" <|
                (content
                    |> Regex.split All (regex "\\s+")
                    |> map (String.split ":+")
                    |> filterMap
                        (\flag ->
                            case flag of
                                [ k, v ] ->
                                    Just ( k, v )

                                [ "" ] ->
                                    Nothing

                                a ->
                                    crash ("Wrong flag format " ++ toString a)
                        )
                    |> foldl (ExContext.addFlag) c
                )

        Normal content ->
            (,) c <|
                (content
                    |> prependAll ("# ")
                    |> indAll c.indent
                )


getCommentType : String -> ElchemyComment
getCommentType comment =
    [ ( "^\\sex\\b", (Ex) )
    , ( "^\\|", (Doc) )
    , ( "^\\sflag\\b", (Flag) )
    ]
        |> List.map (\( a, b ) -> ( Regex.regex a, b ))
        |> List.foldl findCommentType (Normal comment)
=======
{-| Verify correct flag format
-}
verifyFlag : List String -> Maybe ( String, String )
verifyFlag flag =
    case flag of
        [ k, v ] ->
            Just ( k, v )
>>>>>>> dev

        [ "" ] ->
            Nothing

        a ->
            Debug.crash <| "Wrong flag format " ++ toString a


{-| Encode elixir comment and return a context with updated last doc
-}
elixirComment : Context -> String -> ( Context, String )
elixirComment c content =
    case getCommentType content of
        Doc content ->
            if c.hasModuleDoc then
                { c | lastDoc = Just content } => ""
            else
                elixirDoc c ModuleDoc content

        Ex content ->
            (,) c <|
                (content
                    |> String.split "\n"
                    |> List.map String.trim
                    |> String.join "\n"
                    |> indAll c.indent
                )

        Flag content ->
            flip (,) "" <|
                (content
                    |> Regex.split All (regex "\\s+")
                    |> List.map (String.split ":+")
                    |> List.filterMap verifyFlag
                    |> List.foldl (ExContext.addFlag) c
                )

        Normal content ->
            (,) c <|
                (content
                    |> prependAll ("# ")
                    |> indAll c.indent
                )


{-| Enocode a doc and return new context
-}
elixirDoc : Context -> DocType -> String -> ( Context, String )
elixirDoc c doctype content =
    let
        prefix =
            if not c.hasModuleDoc then
                "@moduledoc"
            else if doctype == Fundoc then
                "@doc"
            else
                "@typedoc"
    in
        (,)
            { c
                | hasModuleDoc = True
                , lastDoc = Nothing
            }
        <|
            (ind c.indent)
                ++ prefix
                ++ " \"\"\"\n "
                ++ (content
                        |> String.lines
                        |> List.map (maybeDoctest c)
                        |> List.map (Helpers.escape)
                        |> List.map (Regex.replace All (regex "\"\"\"") (always "\\\"\\\"\\\""))
                        -- |> map trimIndentations
                        |> String.join (ind c.indent)
                    -- Drop an unnecessary \n at the end
                   )
                ++ ind c.indent
                ++ "\"\"\""


{-| Get a type of the comment by it's content
-}
getCommentType : String -> ElchemyComment
getCommentType comment =
    let
        findCommentType regex commentType acc =
            case acc of
                Normal content ->
                    if Regex.contains regex content then
                        commentType <|
                            Regex.replace (Regex.AtMost 1) regex (always "") content
                    else
                        Normal content

                other ->
                    other
    in
        [ ( "^\\sex\\b", (Ex) )
        , ( "^\\|", (Doc) )
        , ( "^\\sflag\\b", (Flag) )
        ]
            |> List.map (\( a, b ) -> ( Regex.regex a, b ))
            |> List.foldl (uncurry findCommentType) (Normal comment)


{-| Encode all exports from a module
-}
subsetExport : ExportSet -> List String
subsetExport exp =
    case exp of
        TypeExport _ _ ->
            []

        FunctionExport name ->
            if isCustomOperator name then
                [ "{:'" ++ translateOperator name ++ "', 0}" ]
            else
                [ "{:'" ++ toSnakeCase True name ++ "', 0}" ]

        _ ->
            Debug.crash ("You can't export " ++ toString exp)


{-| Replace a function doc with a doctest if in correct format
-}
maybeDoctest : Context -> String -> String
maybeDoctest c line =
    if String.startsWith (ind (c.indent + 1)) ("\n" ++ line) then
        case Ast.parseExpression Ast.BinOp.operators (String.trim line) of
            Ok ( _, _, BinOp (Variable [ "==" ]) l r ) ->
                --"\n"
                indNoNewline (c.indent + 1)
                    ++ "iex> import "
                    ++ c.mod
                    ++ ind (c.indent + 2)
                    ++ "iex> "
                    ++ ExExpression.elixirE c l
                    ++ ind (c.indent + 2)
                    ++ ExExpression.elixirE c r
                    ++ "\n"

            _ ->
                line
    else
        line


{-| Get a definition of a type to store it in context
-}
getTypeDefinition : Statement -> Definition
getTypeDefinition a =
    case a of
        FunctionTypeDeclaration name t ->
            let
                arity =
                    typeApplicationToList t |> List.length
            in
                Definition (arity - 1) t

        _ ->
            Debug.crash "It's not a type declaration"


{-| Add type definition into context
-}
addTypeDefinition : Context -> String -> Definition -> Context
addTypeDefinition c name d =
    { c | definitions = Dict.insert name d c.definitions }
