module Pane.Text exposing (..)

import Html exposing (..)
import Html.Attributes as Attribs exposing (..)
import Json.Decode as Json exposing (..)


type alias Model =
    { text : String
    , class : String
    }


type Msg
    = Text String


decodeModel : Decoder Model
decodeModel =
    Json.map2 makeModel
        (field "text" string)
        (field "classes" (Json.list string))

makeModel : String -> List String -> Model
makeModel text classes =
    Model text <| String.concat <| List.intersperse " " classes

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Text text ->
            { model | text = text } ! []


view : Model -> Html Msg
view {text, class} =
    div [ Attribs.class class ]
        [ Html.text text ]
