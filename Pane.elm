module Pane exposing (..)

import Html exposing (..)
import Json.Decode as Json exposing (..)
import Pane.Link as Link
import Pane.Text as Text
import Pane.TextButton as TextButton
import Pane.Title as Title


type Model
    = Link Link.Model
    | Text Text.Model
    | TextButton TextButton.Model
    | Title Title.Model


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


link : Pane Link.Msg Link.Model
link =
    Pane Link.decodeModel Link LinkMsg Link.update Link.view


text : Pane Text.Msg Text.Model
text =
    Pane Text.decodeModel Text TextMsg Text.update Text.view


textButton : Pane TextButton.Msg TextButton.Model
textButton =
    Pane TextButton.decodeModel TextButton TextButtonMsg TextButton.update TextButton.view


title : Pane Title.Msg Title.Model
title =
    Pane Title.decodeModel Title TitleMsg Title.update Title.view


type Msg
    = LinkMsg Link.Msg
    | TextMsg Text.Msg
    | TextButtonMsg TextButton.Msg
    | TitleMsg Title.Msg


decodeModel : String -> Decoder Model
decodeModel type_ =
    case type_ of
        "Link" ->
            Json.map link.makeModel link.decoder

        "Text" ->
            Json.map text.makeModel text.decoder

        "TextButton" ->
            Json.map textButton.makeModel textButton.decoder

        "Title" ->
            Json.map title.makeModel title.decoder

        _ ->
            Json.fail "not a pane"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkMsg linkMsg, Link linkModel ) ->
            updatePane link linkMsg linkModel

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
        Link linkModel ->
            viewPane link linkModel

        Text textModel ->
            viewPane text textModel

        TextButton textButtonModel ->
            viewPane textButton textButtonModel

        Title titleModel ->
            viewPane title titleModel


viewPane : Pane msg model -> model -> Html Msg
viewPane { view, makeMsg } childModel =
    childModel |> view |> Html.map makeMsg
