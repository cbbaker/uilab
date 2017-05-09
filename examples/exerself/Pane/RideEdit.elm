module Pane.RideEdit exposing (..)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes as Attribs exposing (..)
import Html.Events exposing (..)
import Json.Decode as Dec exposing (..)
import Json.Encode as Enc exposing (..)
import UI.Decoders exposing (..)
import Actions
import PubSub


type alias Model =
    { subscription : String
    , data : Data
    , actions : Actions.Model
    }


type alias Validated a =
    { input : String
    , validation : Validation a
    }


validated : a -> Validated a
validated a =
    Validated (toString a) Unvalidated


type Validation a
    = Unvalidated
    | Success a
    | Error String


value : Validated a -> a -> a
value { validation } default =
    case validation of
        Success a ->
            a

        _ ->
            default


encodeDate : Validated Date -> Enc.Value
encodeDate { validation } =
    case validation of
        Success value ->
            Enc.int <| floor ((Date.toTime value) / 1000.0)

        _ ->
            Enc.null


encodeInt : Validated Int -> Enc.Value
encodeInt { validation } =
    case validation of
        Success value ->
            Enc.int value

        _ ->
            Enc.null


encodeString : Validated String -> Enc.Value
encodeString { validation } =
    case validation of
        Success value ->
            Enc.string value

        _ ->
            Enc.null


type alias Data =
    { userId : Int
    , started_at : Validated Date
    , duration : Validated Int
    , power : Validated Int
    , heartRate : Validated Int
    , notes : Validated String
    , newRideId : Maybe String
    }


template : String -> Decoder Model
template id =
    (field "data" decodeData)
        |> Dec.andThen
            (\data ->
                Dec.map (Model (id ++ "Show") data)
                    (field "actions" (decodeActions id data.newRideId))
            )


decodeModel : Environment -> Decoder Model
decodeModel env =
    Dec.map3 Model
        (field "subscription" Dec.string)
        (field "data" (maybeLookupData env))
        (field "actions" Actions.decodeModel)


maybeLookupData : Environment -> Decoder Data
maybeLookupData env =
    case lookup "data" env of
        Nothing ->
            decodeData

        Just value ->
            case decodeValue decodeData value of
                Ok data ->
                    Dec.succeed data

                Err _ ->
                    decodeData


decodeData : Decoder Data
decodeData =
    Dec.map7 Data
        (field "user_id" Dec.int)
        (field "started_at" decodeDate)
        (field "duration" decodeInt)
        (field "power" decodeInt)
        (field "heart_rate" decodeInt)
        (field "notes" decodeString)
        (maybe (field "newRideId" Dec.string))


decodeInt : Decoder (Validated Int)
decodeInt =
    Dec.map (toString >> validateNumber) Dec.int


decodeString : Decoder (Validated String)
decodeString =
    Dec.map validateSuccess Dec.string


decodeDate : Decoder (Validated Date)
decodeDate =
    Dec.map
        (((*) 1000)
            >> Date.fromTime
            >> (\value -> Validated (toString value) (Success value))
        )
        Dec.float


encodeData : Data -> Enc.Value
encodeData { userId, started_at, duration, power, heartRate, notes, newRideId } =
    let
        data =
            Enc.object
                [ ( "user_id", Enc.int userId )
                , ( "started_at", encodeDate started_at )
                , ( "duration", encodeInt duration )
                , ( "power", encodeInt power )
                , ( "heart_rate", encodeInt heartRate )
                , ( "notes", encodeString notes )
                ]
    in
        case newRideId of
            Nothing ->
                data

            Just rideId ->
                Enc.object
                    [ ( rideId
                      , Enc.object
                            [ ( "type", Enc.string "updatedItem" )
                            , ( "data", data )
                            , ( "actions", Enc.object [] )
                            ]
                      )
                    ]


decodeActions : String -> Maybe String -> Decoder Actions.Model
decodeActions id newRideId =
    Dec.map (addActions id newRideId)
        Actions.decodeModel


addActions : String -> Maybe String -> Actions.Model -> Actions.Model
addActions id newRideId =
    let
        showShow id =
            Actions.plainPublishAction id <| Enc.string "show"

        pushModel id =
            Actions.modelPublishAction (id ++ "Edit")

        deleteModel =
            Actions.plainPublishAction "deleteRide" <| Enc.list <| [ Enc.string id ]

        insertModel =
            Actions.modelPublishAction "insertRide"
    in
        case newRideId of
            Nothing ->
                ((Actions.updateModel "cancel" (showShow id))
                    >> (Actions.updateModel "update" (showShow id))
                    >> (Actions.updateModel "update" (pushModel id))
                    >> (Actions.updateModel "delete" deleteModel)
                )

            Just rideId ->
                ((Actions.updateModel "cancel" deleteModel)
                    >> (Actions.updateModel "update" (showShow rideId))
                    >> (Actions.updateModel "update" (pushModel "newItem"))
                )


type Msg
    = Update Data
    | StartedAt String
    | Duration String
    | Power String
    | HeartRate String
    | Notes String
    | SubmitInvalid
    | ActionsMsg Actions.Msg


subscriptions : Model -> Sub Msg
subscriptions { subscription } =
    PubSub.subscribe subscription <| Dec.map Update decodeData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update data ->
            { model | data = data } ! []

        StartedAt started_at ->
            updateDate started_at model

        Duration duration ->
            updateDuration duration model

        Power power ->
            updatePower power model

        HeartRate heartRate ->
            updateHeartRate heartRate model

        Notes notes ->
            updateNotes notes model

        SubmitInvalid ->
            model ! []

        ActionsMsg actionsMsg ->
            updateActions actionsMsg model


updateDate : String -> Model -> ( Model, Cmd Msg )
updateDate started_at model =
    let
        data =
            model.data
    in
        updateData { data | started_at = validateDate started_at } model


updateDuration : String -> Model -> ( Model, Cmd Msg )
updateDuration duration model =
    let
        data =
            model.data
    in
        updateData { data | duration = validateNumber duration } model


updatePower : String -> Model -> ( Model, Cmd Msg )
updatePower power model =
    let
        data =
            model.data
    in
        updateData { data | power = validateNumber power } model


updateHeartRate : String -> Model -> ( Model, Cmd Msg )
updateHeartRate heartRate model =
    let
        data =
            model.data
    in
        updateData { data | heartRate = validateNumber heartRate } model


updateNotes : String -> Model -> ( Model, Cmd Msg )
updateNotes notes model =
    let
        data =
            model.data
    in
        updateData { data | notes = validateSuccess notes } model


updateData : Data -> Model -> ( Model, Cmd Msg )
updateData data model =
    { model | data = data } ! []


validateDate : String -> Validated Date
validateDate input =
    case Date.fromString input of
        Ok date ->
            Validated input <| Success date

        Err err ->
            Validated input (Error err)


validateNonempty : String -> Validated String
validateNonempty input =
    Validated input <|
        if String.length input > 0 then
            Success input
        else
            Error "cannot be empty"


validateNumber : String -> Validated Int
validateNumber input =
    Validated input <|
        case String.toInt input of
            Ok int ->
                Success int

            _ ->
                Error "is not a number"


validateSuccess : String -> Validated String
validateSuccess input =
    Validated input <| Success input


updateActions : Actions.Msg -> Model -> ( Model, Cmd Msg )
updateActions actionsMsg model =
    let
        ( actions, cmds ) =
            Actions.update actionsMsg model.actions
    in
        { model | actions = actions } ! [ Cmd.map ActionsMsg cmds ]


view : Model -> Html Msg
view { data, actions } =
    viewEditing data actions


viewEditing : Data -> Actions.Model -> Html Msg
viewEditing data actions =
    Html.form [ class "list-group-item" ]
        [ dateInput "data[started_at]" "Started at" data.started_at StartedAt
        , numberInput "data[duration]" "Duration" data.duration Duration
        , numberInput "data[power]" "Power" data.power Power
        , numberInput "data[heart_rate]" "Heart rate" data.heartRate HeartRate
        , textAreaInput "data[notes]" "Notes" data.notes Notes
        , controls data actions
        , div [ class "clearfix" ] []
        ]


inputGroup : String -> String -> Validation a -> List (Attribute Msg) -> Html Msg
inputGroup inputName labelText validation inputAttribs =
    let
        ( classString, message ) =
            inputGroupClassList validation

        messageHtml =
            case message of
                Just str ->
                    [ span [ class "help-block" ] [ text str ] ]

                Nothing ->
                    []
    in
        div [ class classString ]
            ([ label [ for inputName ] [ text labelText ]
             , input ((class "form-control") :: (name inputName) :: inputAttribs) []
             ]
                ++ messageHtml
            )


inputGroupClassList : Validation a -> ( String, Maybe String )
inputGroupClassList validation =
    case validation of
        Unvalidated ->
            ( "form-group", Nothing )

        Success _ ->
            ( "form-group has-success", Nothing )

        Error msg ->
            ( "form-group has-error", Just msg )


dateInput : String -> String -> Validated Date -> (String -> Msg) -> Html Msg
dateInput inputName labelText { input, validation } tag =
    inputGroup inputName
        labelText
        validation
        [ type_ "datetime"
        , onInput tag
        , Attribs.value input
        ]


numberInput : String -> String -> Validated Int -> (String -> Msg) -> Html Msg
numberInput inputName labelText { input, validation } tag =
    inputGroup inputName
        labelText
        validation
        [ type_ "number"
        , (Attribs.min "0")
        , (pattern "d*")
        , onInput tag
        , Attribs.value input
        ]


textAreaInput : String -> String -> Validated String -> (String -> Msg) -> Html Msg
textAreaInput inputName labelText { input } tag =
    div [ class "form-group" ]
        [ label [ for inputName ] [ text labelText ]
        , textarea
            [ class "form-control"
            , rows 5
            , onInput tag
            , Attribs.value input
            ]
            []
        ]


valid : Data -> Bool
valid { started_at, duration, heartRate, power, notes } =
    let
        valid { validation } =
            case validation of
                Error _ ->
                    False

                _ ->
                    True
    in
        List.all identity
            [ valid started_at
            , valid duration
            , valid heartRate
            , valid power
            , valid notes
            ]


controls : Data -> Actions.Model -> Html Msg
controls data actions =
    let
        deleteControl =
            if Actions.member "delete" actions then
                [ div [ class "form-group" ]
                    [ a
                        [ href "#"
                        , classList
                            [ ( "btn", True )
                            , ( "btn-danger", True )
                            , ( "pull-left", True )
                            , ( "active", Actions.inProgress "delete" actions )
                            ]
                        , Attribs.attribute "role" "button"
                        , clickEvent "delete"
                        ]
                        [ text "delete" ]
                    ]
                ]
            else
                []

        updateControl =
            if Actions.member "update" actions then
                [ button
                    [ classList
                        [ ( "btn", True )
                        , ( "btn-primary", True )
                        , ( "active", Actions.inProgress "update" actions )
                        , ( "disabled", not (valid data) )
                        ]
                    , type_ "submit"
                    , maybeClickEventWithModel (valid data)
                        "update"
                        (encodeData data)
                    ]
                    [ text "save" ]
                ]
            else
                []

        cancelControl =
            if Actions.member "cancel" actions then
                let
                    click =
                        case data.newRideId of
                            Nothing ->
                                clickEvent "cancel"

                            Just newRideId ->
                                clickEventWithModel "cancel" <| Enc.list [ Enc.string newRideId ]
                in
                    [ a
                        [ href "#"
                        , classList
                            [ ( "btn", True )
                            , ( "btn-default", True )
                            , ( "active", Actions.inProgress "cancel" actions )
                            ]
                        , Attribs.attribute "role" "button"
                        , click
                        ]
                        [ text "back" ]
                    ]
            else
                []

        group =
            div
                [ class "btn-group pull-right"
                , Attribs.attribute "role" "group"
                ]
                (updateControl ++ cancelControl)
    in
        div [ class "form-group" ] (deleteControl ++ [ group ])


clickEvent : String -> Attribute Msg
clickEvent which =
    onClickPreventDefault <| ActionsMsg <| Actions.activate which


clickEventWithModel : String -> Dec.Value -> Attribute Msg
clickEventWithModel which data =
    onClickPreventDefault <| ActionsMsg <| Actions.activateWithModel which data


maybeClickEvent : Bool -> String -> Attribute Msg
maybeClickEvent valid which =
    onClickPreventDefault <|
        if valid then
            ActionsMsg <| Actions.activate which
        else
            SubmitInvalid


maybeClickEventWithModel : Bool -> String -> Dec.Value -> Attribute Msg
maybeClickEventWithModel valid which data =
    onClickPreventDefault <|
        if valid then
            ActionsMsg <| Actions.activateWithModel which data
        else
            SubmitInvalid


onClickPreventDefault : msg -> Attribute msg
onClickPreventDefault msg =
    onWithOptions "click"
        { defaultOptions | preventDefault = True, stopPropagation = True }
        (Dec.succeed msg)
