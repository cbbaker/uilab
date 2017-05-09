module Pane.NewRideButton exposing (..)

import Html exposing (Html, input)
import Html.Attributes as Attribs exposing (classList, type_)
import Html.Events exposing (onClick)
import Json.Decode as Dec exposing (..)
import Json.Encode as Enc exposing (..)
import Date exposing (Date)
import Dict exposing (Dict)
import Task exposing (Task)
import Actions
import PubSub


type alias Model =
    { newItemCount : Int
    , text : String
    , enabled : Bool
    , idPrefix : String
    , defaults : Defaults
    , last : Dec.Value
    , newItemActions : Dec.Value
    , actions : Actions.Model
    }


type alias Defaults =
    Dict String Default


type Default
    = LastCreate
    | CurrentTime
    | ConstInt Int
    | ConstString String


type Msg
    = NewRide
    | InsertRide Date
    | Create Dec.Value
    | ActionsMsg Actions.Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PubSub.subscribe "newRideButton" <| Dec.succeed NewRide
        , PubSub.subscribe "newItemEdit" decodeCreate
        ]


decodeCreate : Decoder Msg
decodeCreate =
    Dec.keyValuePairs value |> Dec.andThen decodeCreatePairs


decodeCreatePairs : List ( a, Enc.Value ) -> Decoder Msg
decodeCreatePairs pairs =
    case pairs of
        ( _, value ) :: _ ->
            case decodeValue decodeCreateValue value of
                Ok defaults ->
                    Dec.succeed defaults

                Err error ->
                    Dec.fail error

        _ ->
            Dec.fail "Couldn't find object in Create"


decodeCreateValue : Decoder Msg
decodeCreateValue =
    Dec.map Create
        (field "data" Dec.value)


decodeModel : Decoder Model
decodeModel =
    Dec.map7 (Model 0)
        (field "text" Dec.string)
        (field "enabled" Dec.bool)
        (field "idPrefix" Dec.string)
        (field "defaults" decodeDefaults)
        (field "last" value)
        (field "newItemActions" value)
        (field "actions" Actions.decodeModel)


decodeDefaults : Decoder Defaults
decodeDefaults =
    Dec.dict decodeDefault


decodeDefault : Decoder Default
decodeDefault =
    (field "type" Dec.string) |> andThen decodeDefaultTypes


decodeDefaultTypes : String -> Decoder Default
decodeDefaultTypes type_ =
    case type_ of
        "current_time" ->
            Dec.succeed CurrentTime

        "const_int" ->
            Dec.map ConstInt (field "value" Dec.int)

        "const_string" ->
            Dec.map ConstString (field "value" Dec.string)

        _ ->
            Dec.succeed LastCreate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRide ->
            model ! [ Task.perform InsertRide Date.now ]

        InsertRide now ->
            updateInsertRide model now

        Create last ->
            { model | last = last } ! []

        ActionsMsg actionsMsg ->
            updateActions actionsMsg model


updateInsertRide : Model -> Date -> ( Model, Cmd Msg )
updateInsertRide model now =
    let
        id =
            model.idPrefix ++ (toString model.newItemCount)

        ride =
            Enc.object
                [ ( "type", Enc.string "newItem" )
                , ( "data", computeDefaults id model now )
                , ( "actions", model.newItemActions )
                ]

        message =
            Enc.object [ ( id, ride ) ]
    in
        { model | newItemCount = (model.newItemCount + 1) }
            ! [ PubSub.publish "insertRide" (Debug.log "insertRide" message) ]


lookup : String -> Dec.Value -> Dec.Value
lookup key object =
    decodeValue (Dec.field key Dec.value) object
        |> Result.withDefault Enc.null


computeDefaults : String -> Model -> Date -> Dec.Value
computeDefaults id { defaults, last } now =
    let
        update name default acc =
            case default of
                LastCreate ->
                    ( name, lookup name last ) :: acc

                CurrentTime ->
                    ( name, encodeDate now ) :: acc

                ConstString string ->
                    ( name, Enc.string string ) :: acc

                ConstInt int ->
                    ( name, Enc.int int ) :: acc
    in
        Enc.object <| Dict.foldl update [ ( "newRideId", Enc.string id ) ] defaults


encodeDate : Date -> Enc.Value
encodeDate date =
    Enc.float ((Date.toTime date) / 1000)


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
