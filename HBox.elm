module HBox exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : List ( String, Html msg ) -> Html msg
view =
    div [ class "row" ] << List.map Tuple.second
