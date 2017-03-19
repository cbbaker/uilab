module Layout.ListGroupItem exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : List ( String, Html msg ) -> Html msg
view =
    div [ class "list-group-item" ] << List.map Tuple.second
