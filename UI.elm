module UI exposing (..)

import Html exposing (Html, div)
import Json.Decode as Json exposing (..)
import Layout
import Choice
import Pane


type Model
    = Layout (Layout.Model Model Msg)
    | Choice (Choice.Model Model)
    | Pane Pane.Model
    | Unknown


type Msg
    = LayoutMsg (Layout.Msg Model Msg)
    | ChoiceMsg (Choice.Msg Msg)
    | PaneMsg Pane.Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Layout layoutModel ->
            layoutModel |> Layout.subscriptions subscriptions |> Sub.map LayoutMsg

        Choice choiceModel ->
            choiceModel |> Choice.subscriptions subscriptions |> Sub.map ChoiceMsg

        Pane paneModel ->
            paneModel |> Pane.subscriptions |> Sub.map PaneMsg

        Unknown ->
            Sub.none


decodeModel : Decoder Model
decodeModel =
    (field "type" string) |> andThen decodeRest


decodeRest : String -> Decoder Model
decodeRest type_ =
    oneOf
        [ Json.map Layout (Layout.decodeModel type_ decodeModel)
        , Json.map Choice (Choice.decodeModel type_ decodeModel)
        , Json.map Pane (Pane.decodeModel type_)
          -- , Json.succeed Unknown
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LayoutMsg layoutMsg, Layout layoutModel ) ->
            let
                ( newLayout, cmd ) =
                    Layout.update update layoutMsg layoutModel
            in
                Layout newLayout ! [ Cmd.map LayoutMsg cmd ]

        ( ChoiceMsg choiceMsg, Choice choiceModel ) ->
            let
                ( newChoice, cmd ) =
                    Choice.update update choiceMsg choiceModel
            in
                Choice newChoice ! [ Cmd.map ChoiceMsg cmd ]

        ( PaneMsg paneMsg, Pane paneModel ) ->
            let
                ( newPane, cmd ) =
                    Pane.update paneMsg paneModel
            in
                Pane newPane ! [ Cmd.map PaneMsg cmd ]

        _ ->
            model ! []


view : Model -> Html Msg
view model =
    case model of
        Layout layoutModel ->
            layoutModel |> Layout.view view |> Html.map LayoutMsg

        Choice choiceModel ->
            choiceModel |> Choice.view view |> Html.map ChoiceMsg

        Pane paneModel ->
            paneModel |> Pane.view |> Html.map PaneMsg

        Unknown ->
            div [] []
