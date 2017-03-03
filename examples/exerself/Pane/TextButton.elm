module Pane.TextButton exposing (..)

import Html exposing (Html, input)
import Html.Attributes as Attribs exposing (classList, type_)
import Html.Events exposing (onClick)
import Json.Decode as Json exposing (..)
import Actions

type alias Model =
    { text : String
    , actions : Actions.Model
    }


type Msg
    = ActionsMsg Actions.Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


decodeModel : Decoder Model
decodeModel =
    map2 Model
        (field "text" string)
        (field "actions" Actions.decodeModel)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ActionsMsg actionsMsg ->
            updateActions actionsMsg model

updateActions : Actions.Msg -> Model -> ( Model, Cmd Msg )
updateActions actionsMsg model =
    let
        ( actions, cmds ) = Actions.update actionsMsg model.actions
    in
        { model | actions = actions } ! [ Cmd.map ActionsMsg cmds ]



view : Model -> Html Msg
view { text, actions } =
    input
        [ type_ "button"
        , onClick <| ActionsMsg <| Actions.activate "press"
        , classList
            [ ( "btn", True )
            , ( "btn-large", True )
            , ( "btn-default", True )
            , ( "active", Actions.inProgress "press" actions )
            ]
        , Attribs.value text
        ]
        []
