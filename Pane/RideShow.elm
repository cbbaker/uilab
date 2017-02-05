module Pane.RideShow exposing (..)

import Html exposing (..)
import Html.Attributes as Attribs exposing (..)
import Html.Events exposing (..)
import Json.Decode as Dec exposing (..)
import Json.Encode as Enc exposing (..)
import PubSub
import Actions


type alias Model =
    { subscription : String
    , data : Data
    , actions : Actions.Model
    }


type alias Data =
    { userId : Int
    , date : String
    , duration : Int
    , power : Int
    , heartRate : Int
    , notes : String
    }


decodeModel : Decoder Model
decodeModel =
    Dec.map3 Model
        (field "subscription" Dec.string)
        (field "data" decodeData)
        (field "actions" Actions.decodeModel)


decodeData : Decoder Data
decodeData =
    Dec.map6 Data
        (field "user_id" Dec.int)
        (field "date" Dec.string)
        (field "duration" Dec.int)
        (field "power" Dec.int)
        (field "heart_rate" Dec.int)
        (field "notes" Dec.string)


encodeData : Data -> Enc.Value
encodeData { userId, date, duration, power, heartRate, notes } =
    Enc.object
        [ ( "user_id", Enc.int userId )
        , ( "date", Enc.string date )
        , ( "duration", Enc.int duration )
        , ( "power", Enc.int power )
        , ( "heart_rate", Enc.int heartRate )
        , ( "notes", Enc.string notes )
        ]


encodeInt : String -> Enc.Value
encodeInt =
    String.toInt >> Result.withDefault 0 >> Enc.int


type Msg
    = Update Data
    | ActionsMsg Actions.Msg


subscriptions : Model -> Sub Msg
subscriptions { subscription } =
    PubSub.subscribe subscription <| Dec.map Update decodeData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update data ->
            updateUpdate data model

        ActionsMsg actionsMsg ->
            updateActions actionsMsg model


updateUpdate : Data -> Model -> ( Model, Cmd Msg )
updateUpdate data model =
    { model | data = data } ! []


updateActions : Actions.Msg -> Model -> ( Model, Cmd Msg )
updateActions actionsMsg model =
    let
        ( actions, cmds ) =
            Actions.update actionsMsg model.actions
    in
        { model | actions = actions } ! [ Cmd.map ActionsMsg cmds ]


view : Model -> Html Msg
view { data, actions } =
    viewShow data actions


viewShow : Data -> Actions.Model -> Html Msg
viewShow data actions =
    let
        { date, duration, power, heartRate, notes } =
            data

        formatDuration =
            (toString duration) ++ " min"

        formatPower =
            (toString power) ++ " W"

        formatHeartRate =
            (toString heartRate) ++ " bpm"

        formatEfficiency =
            (toString ((toFloat (round ((toFloat power * 1000) / (toFloat heartRate)))) / 1000)) ++ "%"
    in
        a
            [ href "#"
            , classList
                [ ( "list-group-item", True )
                , ( "active", Actions.inProgress "edit" actions )
                ]
            , attribute "role" "button"
            , onClickPreventDefault <| ActionsMsg <| Actions.activateWithModel "edit" <| encodeData data
            ]
            [ div [ class "row" ]
                [ div [ class "col-xs-8 text-left" ] [ text date ]
                , div [ class "col-xs-4 text-right" ] [ text formatDuration ]
                ]
            , div [ class "row" ]
                [ div [ class "col-xs-4 text-left" ] [ text formatPower ]
                , div [ class "col-xs-4 text-center" ] [ text formatHeartRate ]
                , div [ class "col-xs-4 text-right" ] [ text formatEfficiency ]
                ]
            , div [ class "row" ]
                [ div [ class "col-xs-12 text-muted" ] [ text notes ]
                ]
            ]


onClickPreventDefault : msg -> Attribute msg
onClickPreventDefault msg =
    onWithOptions "click" { defaultOptions | preventDefault = True } (Dec.succeed msg)
