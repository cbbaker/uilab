module Layout.ListGroup exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : List ( String, Html msg ) -> Html msg
view =
    div [ class "list-group" ] << List.map Tuple.second
