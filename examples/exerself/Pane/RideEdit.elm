module Pane.RideEdit exposing (..)

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
    { value : a
    , validation : Validation
    }


validated : a -> Validated a
validated a =
    Validated a Unvalidated


type Validation
    = Unvalidated
    | Success
    | Error String


type alias Data =
    { userId : Int
    , started_at : Validated String
    , duration : Validated String
    , power : Validated String
    , heartRate : Validated String
    , notes : Validated String
    , newRideId : Maybe String
    }


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
        (field "started_at" decodeString)
        (field "duration" decodeInt)
        (field "power" decodeInt)
        (field "heart_rate" decodeInt)
        (field "notes" decodeString)
        (maybe (field "newRideId" Dec.string))


decodeInt : Decoder (Validated String)
decodeInt =
    Dec.map (toString >> validated) Dec.int


decodeString : Decoder (Validated String)
decodeString =
    Dec.map validated Dec.string


encodeData : Data -> Enc.Value
encodeData { userId, started_at, duration, power, heartRate, notes } =
    Enc.object
        [ ( "user_id", Enc.int userId )
        , ( "started_at", Enc.string started_at.value )
        , ( "duration", encodeInt duration.value )
        , ( "power", encodeInt power.value )
        , ( "heart_rate", encodeInt heartRate.value )
        , ( "notes", Enc.string notes.value )
        ]


encodeInt : String -> Enc.Value
encodeInt =
    String.toInt >> Result.withDefault 0 >> Enc.int


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
        updateData { data | started_at = validateNonempty started_at } model


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


validateNonempty : String -> Validated String
validateNonempty input =
    Validated input <|
        if String.length input > 0 then
            Success
        else
            Error "cannot be empty"


validateNumber : String -> Validated String
validateNumber input =
    Validated input <|
        case String.toInt input of
            Ok _ ->
                Success

            _ ->
                Error "is not a number"


validateSuccess : String -> Validated String
validateSuccess input =
    Validated input Success


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


inputGroup : String -> String -> Validation -> List (Attribute Msg) -> Html Msg
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


inputGroupClassList : Validation -> ( String, Maybe String )
inputGroupClassList validation =
    case validation of
        Unvalidated ->
            ( "form-group", Nothing )

        Success ->
            ( "form-group has-success", Nothing )

        Error msg ->
            ( "form-group has-error", Just msg )


dateInput : String -> String -> Validated String -> (String -> Msg) -> Html Msg
dateInput inputName labelText { value, validation } tag =
    inputGroup inputName
        labelText
        validation
        [ type_ "datetime"
        , onInput tag
        , Attribs.value value
        ]


numberInput : String -> String -> Validated String -> (String -> Msg) -> Html Msg
numberInput inputName labelText { value, validation } tag =
    inputGroup inputName
        labelText
        validation
        [ type_ "number"
        , (Attribs.min "0")
        , (pattern "d*")
        , onInput tag
        , Attribs.value value
        ]


textAreaInput : String -> String -> Validated String -> (String -> Msg) -> Html Msg
textAreaInput inputName labelText { value } tag =
    div [ class "form-group" ]
        [ label [ for inputName ] [ text labelText ]
        , textarea
            [ class "form-control"
            , rows 5
            , onInput tag
            , Attribs.value value
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
        List.all valid [ started_at, duration, heartRate, power, notes ]


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
                      ] [ text "back" ]
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
