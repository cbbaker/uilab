module UI exposing (..)

import Html exposing (Html, div)
import Json.Decode as Json exposing (..)
import Layout
import Pane


type Model
    = Layout (Layout.Model Model Msg)
    | Pane Pane.Model
    | Unknown


type Msg
    = LayoutMsg (Layout.Msg Msg)
    | PaneMsg Pane.Msg


decodeModel : Decoder Model
decodeModel =
    (field "type" string) |> andThen decodeRest


decodeRest : String -> Decoder Model
decodeRest type_ =
    oneOf
        [ Json.map Layout (Layout.decodeModel type_ decodeModel)
        , Json.map Pane (Pane.decodeModel type_)
        , Json.succeed Unknown
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

        Pane paneModel ->
            paneModel |> Pane.view |> Html.map PaneMsg

        Unknown ->
            div [] []
