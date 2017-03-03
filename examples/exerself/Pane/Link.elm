module Pane.Link exposing (..)

import Html exposing (..)
import Html.Attributes as Attribs exposing (..)
import Json.Decode as Json exposing (..)


type alias Model =
    { text : String
    , url : String
    , class : String
    }


type Msg
    = Text String


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


decodeModel : Decoder Model
decodeModel =
    Json.map3 makeModel
        (field "text" string)
        (field "url" string)
        (field "classes" (Json.list string))


makeModel : String -> String -> List String -> Model
makeModel text url classes =
    Model text url <| String.concat <| List.intersperse " " classes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Text text ->
            { model | text = text } ! []


view : Model -> Html Msg
view { text, url, class } =
    a
        [ href url
        , Attribs.class class
        ]
        [ Html.text text ]
