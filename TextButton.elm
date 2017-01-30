module TextButton exposing (..)

import Html exposing (Html, input)
import Html.Attributes as Attribs exposing (classList, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing (..)


type alias Model =
    { text : String
    , url : String
    , method : String
    , body : Json.Value
    , inProgress : Bool
    }


type Msg
    = Press
    | PressResult (Result Http.Error String)


decodeModel : Decoder Model
decodeModel =
    map5 Model
        (field "text" string)
        (field "url" string)
        (field "method" string)
        (field "body" Json.value)
        (succeed False)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Press ->
            { model | inProgress = True }
                ! if model.inProgress then
                    []
                  else
                    [ ajax model ]

        PressResult _ ->
            { model | inProgress = False } ! []


ajax : Model -> Cmd Msg
ajax { url, method, body } =
    let
        request =
            Http.request
                { method = method
                , headers = []
                , url = url
                , body = Http.jsonBody body
                , expect = Http.expectString
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send PressResult request


view : Model -> Html Msg
view { text, inProgress } =
    input
        [ type_ "button"
        , onClick Press
        , classList
            [ ( "btn", True )
            , ( "btn-large", True )
            , ( "btn-default", True )
            , ( "active", inProgress )
            ]
        , Attribs.value text
        ]
        []
