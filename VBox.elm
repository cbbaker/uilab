module VBox exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : List ( String, Html msg ) -> Html msg
view =
    div [ class "container" ] << List.map Tuple.second
