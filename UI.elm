module UI exposing (..)

import Html exposing (..)
import Json.Decode as Json exposing (..)
import Layout
import Text
import TextButton


type Model
    = Layout (Layout.Model Model Msg)
    | Text Text.Model
    | TextButton TextButton.Model
    | Unknown


type alias PaneUpdater msg model =
    msg -> model -> ( model, Cmd msg )


type alias PaneViewer msg model =
    model -> Html msg


type alias Pane msg model =
    { decoder : Decoder model
    , makeModel : model -> Model
    , makeMsg : msg -> Msg
    , update : PaneUpdater msg model
    , view : PaneViewer msg model
    }


text : Pane Text.Msg Text.Model
text =
    Pane Text.decodeModel Text TextMsg Text.update Text.view


textButton : Pane TextButton.Msg TextButton.Model
textButton =
    Pane TextButton.decodeModel TextButton TextButtonMsg TextButton.update TextButton.view


type Msg
    = LayoutMsg (Layout.Msg Msg)
    | TextMsg Text.Msg
    | TextButtonMsg TextButton.Msg


decodeModel : Decoder Model
decodeModel =
    (field "type" string) |> andThen decodeRest


decodeRest : String -> Decoder Model
decodeRest type_ =
    oneOf
        [ Json.map Layout (Layout.decodeModel type_ decodeModel)
        , decodePane type_
        ]


decodePane : String -> Decoder Model
decodePane type_ =
    case type_ of
        "Text" ->
            Json.map text.makeModel text.decoder

        "TextButton" ->
            Json.map textButton.makeModel textButton.decoder

        _ ->
            Json.succeed Unknown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LayoutMsg layoutMsg, Layout layoutModel ) ->
            let
                ( newLayout, cmd ) =
                    Layout.update update layoutMsg layoutModel
            in
                Layout newLayout ! [ Cmd.map LayoutMsg cmd ]

        ( TextMsg textMsg, Text textModel ) ->
            updatePane text textMsg textModel

        ( TextButtonMsg textButtonMsg, TextButton textButtonModel ) ->
            updatePane textButton textButtonMsg textButtonModel

        _ ->
            model ! []


updatePane : Pane msg model -> msg -> model -> ( Model, Cmd Msg )
updatePane { update, makeModel, makeMsg } childMsg childModel =
    let
        ( newModel, cmd ) =
            update childMsg childModel
    in
        makeModel newModel ! [ Cmd.map makeMsg cmd ]


view : Model -> Html Msg
view model =
    case model of
        Layout layoutModel ->
            layoutModel |> Layout.view view |> Html.map LayoutMsg

        Text textModel ->
            viewPane text textModel

        TextButton textButtonModel ->
            viewPane textButton textButtonModel

        Unknown ->
            div [] []


viewPane : Pane msg model -> model -> Html Msg
viewPane { view, makeMsg } childModel =
    childModel |> view |> Html.map makeMsg
