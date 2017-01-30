module UI exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json exposing (..)
import Text
import TextButton


type Model
    = VBox VBoxModel
    | PullRight Model
    | Text Text.Model
    | TextButton TextButton.Model
    | Unknown


type alias VBoxModel =
    List Model


type Msg
    = VBoxMsg Int Msg
    | PullRightMsg Msg
    | TextMsg Text.Msg
    | TextButtonMsg TextButton.Msg


decodeModel : Decoder Model
decodeModel =
    (field "type" string) |> andThen decodeRest


decodeRest : String -> Decoder Model
decodeRest type_ =
    case (Debug.log "parsing" type_) of
        "VBox" ->
            Json.map VBox <| decodeVBox

        "PullRight" ->
            Json.map PullRight <| decodePullRight

        "Text" ->
            Json.map Text <| Text.decodeModel

        "TextButton" ->
            Json.map TextButton <| TextButton.decodeModel

        _ ->
            (Debug.log "found unknown node" type_) |> (always (Json.succeed Unknown))


decodeVBox : Decoder VBoxModel
decodeVBox =
    (field "children" (Json.list (lazy (\_ -> decodeModel))))


decodePullRight : Decoder Model
decodePullRight =
    (field "child" (lazy (\_ -> decodeModel)))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( VBoxMsg i vboxMsg, VBox vboxModel ) ->
            updateVBox i vboxMsg vboxModel

        ( PullRightMsg pullRightMsg, PullRight pullRightModel ) ->
            updatePullRight pullRightMsg pullRightModel

        ( TextMsg textMsg, Text textModel ) ->
            updateText textMsg textModel

        ( TextButtonMsg textButtonMsg, TextButton textButtonModel ) ->
            updateTextButton textButtonMsg textButtonModel

        _ ->
            model ! []


updateVBox : Int -> Msg -> List Model -> ( Model, Cmd Msg )
updateVBox i msg models =
    let
        updateModel j restModels =
            case restModels of
                head :: tail ->
                    if j == i then
                        let
                            ( newModel, cmd ) =
                                update msg head
                        in
                            (newModel :: tail) ! [ Cmd.map (VBoxMsg i) cmd ]
                    else
                        let
                            ( newModels, cmd ) =
                                updateModel (j + 1) tail
                        in
                            ( head :: newModels, cmd )

                [] ->
                    models ! []
    in
        let
            ( newModels, cmd ) =
                updateModel i models
        in
            VBox newModels ! [ cmd ]


updatePullRight : Msg -> Model -> ( Model, Cmd Msg )
updatePullRight msg model =
    let
        ( newModel, cmd ) =
            update msg model
    in
        PullRight newModel ! [ Cmd.map PullRightMsg cmd ]


updateText : Text.Msg -> Text.Model -> ( Model, Cmd Msg )
updateText msg model =
    let
        ( newModel, cmd ) =
            Text.update msg model
    in
        Text newModel ! [ Cmd.map TextMsg cmd ]


updateTextButton : TextButton.Msg -> TextButton.Model -> ( Model, Cmd Msg )
updateTextButton msg model =
    let
        ( newModel, cmd ) =
            TextButton.update msg model
    in
        TextButton newModel ! [ Cmd.map TextButtonMsg cmd ]


view : Model -> Html Msg
view model =
    case model of
        VBox vboxModel ->
            viewVBox vboxModel

        PullRight pullRightModel ->
            viewPullRight pullRightModel

        Text textModel ->
            viewText textModel

        TextButton textButtonModel ->
            viewTextButton textButtonModel

        Unknown ->
            div [] []


viewVBox : VBoxModel -> Html Msg
viewVBox models =
    let
        viewRec i restModel =
            case restModel of
                head :: tail ->
                    let
                        row =
                            div [ class "row" ] [ view head ]
                    in
                        (Html.map (VBoxMsg i) row) :: viewRec (i + 1) tail

                [] ->
                    []
    in
        div [ class "container" ] <| viewRec 0 models


viewPullRight : Model -> Html Msg
viewPullRight model =
    div [ class "pull-right" ] [ Html.map PullRightMsg <| view model ]


viewText : Text.Model -> Html Msg
viewText model =
    Html.map TextMsg <| Text.view model


viewTextButton : TextButton.Model -> Html Msg
viewTextButton model =
    Html.map TextButtonMsg <| TextButton.view model
