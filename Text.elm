module Text exposing (..)

import Html exposing (Html, h2)
import Json.Decode as Json exposing (..)


type alias Model =
    { text : String
    }


type Msg
    = Text String


decodeModel : Decoder Model
decodeModel =
    Json.map Model
        (field "text" string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Text text ->
            { model | text = text } ! []


view : Model -> Html Msg
view { text } =
    h2 []
        [ Html.text text ]
