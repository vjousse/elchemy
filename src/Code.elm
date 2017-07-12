module Code exposing (..)

import ExContext exposing (Context)


type alias Code =
    { context : Context
    , product : String
    , error : Maybe String
    }


empty : Context -> Code
empty c =
    Code c "" Nothing


map : (Code -> Code) -> Code -> Code
map f c =
    case c.error of
        Nothing ->
            f c

        Just _ ->
            c


infixl 0 |+
(|+) : Code -> String -> Code
(|+) c string =
    c |> map (\c -> { c | product = c.product ++ string })


infixl 0 <||>
(<||>) : Code -> (Context -> Context) -> Code
(<||>) c mapContext =
    c |> map (\c -> { c | context = mapContext c.context })


infixl 0 <|+>
(<|+>) : Code -> (Context -> Context) -> Code
(<|+>) c mapContext =
    c |> map (\c -> { c | context = mapContext c.context })


infixl 0 <//>
(<//>) : Code -> String -> Code
(<//>) c error =
    c |> map (\c -> { c | error = Just error })
