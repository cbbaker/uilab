module Pane.NewRideButton exposing (..)

import Html exposing (Html, input)
import Html.Attributes as Attribs exposing (classList, type_)
import Html.Events exposing (onClick)
import Json.Decode as Json exposing (..)
import Actions
import PubSub


type alias Model =
    { text : String
    , enabled : Bool
    , actions : Actions.Model
    }


type Msg
    = Enabled Bool
    | ActionsMsg Actions.Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    PubSub.subscribe "newRideButton" <|
        Json.map Enabled (field "enabled" bool)


decodeModel : Decoder Model
decodeModel =
    map3 Model
        (field "text" string)
        (field "enabled" bool)
        (field "actions" Actions.decodeModel)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Enabled enabled ->
            { model | enabled = enabled } ! []

        ActionsMsg actionsMsg ->
            updateActions actionsMsg model


updateActions : Actions.Msg -> Model -> ( Model, Cmd Msg )
updateActions actionsMsg model =
    let
        ( actions, cmds ) =
            Actions.update actionsMsg model.actions
    in
        { model | actions = actions } ! [ Cmd.map ActionsMsg cmds ]


view : Model -> Html Msg
view { text, enabled, actions } =
    let
        attribs =
            [ type_ "button"
            , classList
                [ ( "btn", True )
                , ( "btn-large", True )
                , ( "btn-default", True )
                , ( "active", Actions.inProgress "press" actions )
                , ( "disabled", not enabled )
                ]
            , Attribs.value text
            ]

        withEvent =
            if enabled then
                (onClick <| ActionsMsg <| Actions.activate "press") :: attribs
            else
                attribs
    in
        input withEvent []
