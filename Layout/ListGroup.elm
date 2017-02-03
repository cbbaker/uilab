module Layout.ListGroup exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : List ( String, Html msg ) -> Html msg
view =
    ul [ class "list-group" ] << List.map Tuple.second
