module Pane.RideShow exposing (..)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes as Attribs exposing (..)
import Html.Events exposing (..)
import Json.Decode as Dec exposing (..)
import Json.Encode as Enc exposing (..)
import PubSub
import Actions


type alias Model =
    { uri : String
    , data : Data
    , actions : Actions.Model
    }


type alias Data =
    { userId : Int
    , started_at : Date
    , duration : Int
    , power : Int
    , heartRate : Int
    , notes : String
    }


template : Decoder Model
template =
    Dec.map3 makeModel
        (field "uri" Dec.string)
        (field "data" decodeData)
        (field "actions" Actions.decodeModel)


makeModel : String -> Data -> Actions.Model -> Model
makeModel uri data actions =
    Model uri data <| addActions uri actions


decodeData : Decoder Data
decodeData =
    Dec.map6 Data
        (field "user_id" Dec.int)
        (field "started_at" decodeDate)
        (field "duration" Dec.int)
        (field "power" Dec.int)
        (field "heart_rate" Dec.int)
        (field "notes" Dec.string)


decodeDate : Decoder Date
decodeDate =
    Dec.map (((*) 1000) >> Date.fromTime) Dec.float


encodeData : Data -> Enc.Value
encodeData { userId, started_at, duration, power, heartRate, notes } =
    Enc.object
        [ ( "user_id", Enc.int userId )
        , ( "started_at", encodeDate started_at )
        , ( "duration", Enc.int duration )
        , ( "power", Enc.int power )
        , ( "heart_rate", Enc.int heartRate )
        , ( "notes", Enc.string notes )
        ]


encodeDate : Date -> Enc.Value
encodeDate date =
    Enc.int <| floor ((Date.toTime date) / 1000)


addActions : String -> Actions.Model -> Actions.Model
addActions uri =
    let
        showEdit =
            Actions.plainPublishAction uri <| Enc.string "edit"

        pushModel =
            Actions.modelPublishAction (uri ++ "/show")

        deleteModel =
            Actions.plainPublishAction "deleteRide" <| Enc.list <| [ Enc.string uri ]
    in
        (Actions.updateModel "edit" showEdit)
            >> (Actions.updateModel "edit" pushModel)
            >> (Actions.updateModel "delete" deleteModel)


type Msg
    = Update Data
    | ActionsMsg Actions.Msg


subscriptions : Model -> Sub Msg
subscriptions { uri } =
    PubSub.subscribe (uri ++ "/edit") <| Dec.map Update decodeData


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
        { started_at, duration, power, heartRate, notes } =
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
                [ div [ class "col-xs-8 text-left" ] [ text <| toString started_at ]
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
