module Layout exposing (..)

import Html exposing (..)
import Json.Decode as Json exposing (..)
import Dict exposing (Dict)
import VBox
import HBox
import PullRight


type alias Viewer uiMsg =
    List ( String, Html (Msg uiMsg) ) -> Html (Msg uiMsg)


type alias UIList ui =
    List ( String, ui )


type alias Model ui uiMsg =
    { view : Viewer uiMsg
    , children : UIList ui
    }


type Msg uiMsg
    = Msg String uiMsg


layouts : Dict String (Viewer uiMsg)
layouts =
    Dict.fromList
        [ ( "VBox", VBox.view )
        , ( "HBox", HBox.view )
        , ( "PullRight", PullRight.view )
        ]


decodeModel : String -> Decoder ui -> Decoder (Model ui uiMsg)
decodeModel type_ decodeUI =
    case Dict.get type_ layouts of
        Nothing ->
            Json.fail "not a layout"

        Just view ->
            decodeChildren view decodeUI


decodeChildren : Viewer uiMsg -> Decoder ui -> Decoder (Model ui uiMsg)
decodeChildren view decoder =
    Json.map ((Model view) << List.reverse)
        (field "children" (Json.keyValuePairs (lazy (\_ -> decoder))))


update :
    (uiMsg -> ui -> ( ui, Cmd uiMsg ))
    -> Msg uiMsg
    -> Model ui uiMsg
    -> ( Model ui uiMsg, Cmd (Msg uiMsg) )
update updateUI (Msg key msg) { view, children } =
    Tuple.mapFirst (Model view) <| updateChildren updateUI key msg children


updateChildren :
    (uiMsg -> ui -> ( ui, Cmd uiMsg ))
    -> String
    -> uiMsg
    -> UIList ui
    -> ( UIList ui, Cmd (Msg uiMsg) )
updateChildren uiUpdate key msg models =
    let
        combine key ( newUI, newCmd ) ( uis, cmd ) =
            ( ( key, newUI ) :: uis
            , Cmd.batch
                [ Cmd.map (Msg key) newCmd
                , cmd
                ]
            )

        update ( k, old ) result =
            let
                current =
                    if k == key then
                        uiUpdate msg old
                    else
                        ( old, Cmd.none )
            in
                combine key current result
    in
        List.foldr update ( [], Cmd.none ) models


view : (model -> Html msg) -> Model model msg -> Html (Msg msg)
view viewUI { view, children } =
    let
        viewChild child =
            let
                ( key, value ) =
                    child
            in
                ( key, Html.map (Msg key) <| viewUI value )
    in
        children |> List.map viewChild |> view
