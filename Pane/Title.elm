module Pane.Title exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json exposing (..)


type alias Model =
    { text : String
    , level : Int
    , classes : List String
    }


type Msg
    = Text String


decodeModel : Decoder Model
decodeModel =
    Json.map3 Model
        (field "text" string)
        (field "level" int)
        (field "classes" (Json.list string))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Text text ->
            { model | text = text } ! []


tag : Int -> List (Attribute msg) -> List (Html msg) -> Html msg
tag level =
    case level of
        1 ->
            h1

        2 ->
            h2

        3 ->
            h3

        4 ->
            h4

        5 ->
            h5

        6 ->
            h6

        _ ->
            p


view : Model -> Html Msg
view { text, level, classes } =
    let
        flattened =
            List.map (\i -> ( i, True )) classes
    in
        tag level
            [ classList flattened ]
            [ Html.text text ]
