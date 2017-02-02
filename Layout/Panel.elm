module Layout.Panel exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : List ( String, Html msg ) -> Html msg
view children =
    let
        lookup key children =
            case children of
                (k, v) :: rest  ->
                    if k == key then
                        Just v
                    else
                        lookup key rest

                _ ->
                    Nothing

        wrap classNames node = [ div [ class classNames ] [ node ] ]

        heading =
            children |> lookup "heading" |> Maybe.map (wrap "panel-heading") |> Maybe.withDefault []

        body =
            children |> lookup "body" |> Maybe.map (wrap "panel-body") |> Maybe.withDefault []

        footer =
            children |> lookup "footer" |> Maybe.map (wrap "panel-footer") |> Maybe.withDefault []
                        
    in 
        div [ class "panel panel-default" ] ( heading ++ body ++ footer )
