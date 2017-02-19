module Pane exposing (..)

import Html exposing (..)
import Json.Decode as Json exposing (..)
import Pane.Link as Link
import Pane.Text as Text
import Pane.Flash as Flash
import Pane.TextButton as TextButton
import Pane.NewRideButton as NewRideButton
import Pane.Title as Title
import Pane.RideShow as RideShow
import Pane.RideEdit as RideEdit


type Model
    = Link (Meta Link.Model Link.Msg) Link.Model
    | Text (Meta Text.Model Text.Msg) Text.Model
    | Flash (Meta Flash.Model Flash.Msg) Flash.Model
    | TextButton (Meta TextButton.Model TextButton.Msg) TextButton.Model
    | NewRideButton (Meta NewRideButton.Model NewRideButton.Msg) NewRideButton.Model
    | Title (Meta Title.Model Title.Msg) Title.Model
    | RideShow (Meta RideShow.Model RideShow.Msg) RideShow.Model
    | RideEdit (Meta RideEdit.Model RideEdit.Msg) RideEdit.Model


type alias MakeModel model msg =
    Meta model msg -> model -> Model


type alias MakeMsg msg =
    msg -> Msg


type alias Update model msg =
    msg -> model -> ( model, Cmd msg )


type alias View model msg =
    model -> Html msg


type Meta model msg
    = Meta (MakeModel model msg) (MakeMsg msg) (Update model msg) (View model msg)


type Msg
    = LinkMsg Link.Msg
    | TextMsg Text.Msg
    | FlashMsg Flash.Msg
    | TextButtonMsg TextButton.Msg
    | NewRideButtonMsg NewRideButton.Msg
    | TitleMsg Title.Msg
    | RideShowMsg RideShow.Msg
    | RideEditMsg RideEdit.Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Link _ linkModel ->
            linkModel |> Link.subscriptions |> Sub.map LinkMsg

        Text _ textModel ->
            textModel |> Text.subscriptions |> Sub.map TextMsg

        Flash _ flashModel ->
            flashModel |> Flash.subscriptions |> Sub.map FlashMsg

        TextButton _ textButtonModel ->
            textButtonModel |> TextButton.subscriptions |> Sub.map TextButtonMsg

        NewRideButton _ newRideButtonModel ->
            newRideButtonModel |> NewRideButton.subscriptions |> Sub.map NewRideButtonMsg

        Title _ titleModel ->
            titleModel |> Title.subscriptions |> Sub.map TitleMsg

        RideShow _ rideShowModel ->
            rideShowModel |> RideShow.subscriptions |> Sub.map RideShowMsg

        RideEdit _ rideEditModel ->
            rideEditModel |> RideEdit.subscriptions |> Sub.map RideEditMsg


decodeModel : String -> Decoder Model
decodeModel type_ =
    case type_ of
        "Link" ->
            Json.map (Link (Meta Link LinkMsg Link.update Link.view))
                Link.decodeModel

        "Text" ->
            Json.map (Text (Meta Text TextMsg Text.update Text.view))
                Text.decodeModel

        "Flash" ->
            Json.map (Flash (Meta Flash FlashMsg Flash.update Flash.view))
                Flash.decodeModel

        "TextButton" ->
            Json.map (TextButton (Meta TextButton TextButtonMsg TextButton.update TextButton.view))
                TextButton.decodeModel

        "NewRideButton" ->
            Json.map (NewRideButton (Meta NewRideButton NewRideButtonMsg NewRideButton.update NewRideButton.view))
                NewRideButton.decodeModel

        "Title" ->
            Json.map (Title (Meta Title TitleMsg Title.update Title.view))
                Title.decodeModel

        "RideShow" ->
            Json.map (RideShow (Meta RideShow RideShowMsg RideShow.update RideShow.view))
                RideShow.decodeModel

        "RideEdit" ->
            Json.map (RideEdit (Meta RideEdit RideEditMsg RideEdit.update RideEdit.view))
                RideEdit.decodeModel

        _ ->
            Json.fail (Debug.log "unknown pane" type_)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkMsg linkMsg, Link meta linkModel ) ->
            updatePane linkMsg meta linkModel

        ( TextMsg textMsg, Text meta textModel ) ->
            updatePane textMsg meta textModel

        ( FlashMsg flashMsg, Flash meta flashModel ) ->
            updatePane flashMsg meta flashModel

        ( TextButtonMsg textButtonMsg, TextButton meta textButtonModel ) ->
            updatePane textButtonMsg meta textButtonModel

        ( NewRideButtonMsg newRideButtonMsg, NewRideButton meta newRideButtonModel ) ->
            updatePane newRideButtonMsg meta newRideButtonModel

        ( TitleMsg titleMsg, Title meta titleModel ) ->
            updatePane titleMsg meta titleModel

        ( RideShowMsg rideShowMsg, RideShow meta rideShowModel ) ->
            updatePane rideShowMsg meta rideShowModel

        ( RideEditMsg rideEditMsg, RideEdit meta rideEditModel ) ->
            updatePane rideEditMsg meta rideEditModel

        _ ->
            model ! []


updatePane : msg -> Meta model msg -> model -> ( Model, Cmd Msg )
updatePane childMsg (Meta makeModel makeMsg update view) model =
    let
        ( newModel, cmd ) =
            update childMsg model
    in
        makeModel (Meta makeModel makeMsg update view) newModel ! [ Cmd.map makeMsg cmd ]


view : Model -> Html Msg
view model =
    case model of
        Link meta linkModel ->
            viewPane meta linkModel

        Text meta textModel ->
            viewPane meta textModel

        Flash meta flashModel ->
            viewPane meta flashModel

        TextButton meta textButtonModel ->
            viewPane meta textButtonModel

        NewRideButton meta newRideButtonModel ->
            viewPane meta newRideButtonModel

        Title meta titleModel ->
            viewPane meta titleModel

        RideShow meta rideShowModel ->
            viewPane meta rideShowModel

        RideEdit meta rideEditModel ->
            viewPane meta rideEditModel


viewPane : Meta model msg -> model -> Html Msg
viewPane (Meta _ makeMsg _ view) childModel =
    childModel |> view |> Html.map makeMsg
