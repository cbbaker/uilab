module Layout.PullRight exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : List ( String, Html msg ) -> Html msg
view =
    div [ class "pull-right" ] << List.map Tuple.second
