module Layout exposing (..)

import Html exposing (..)
import Json.Decode as Json exposing (..)
import Dict exposing (Dict)
import Layout.Panel
import Layout.Container
import Layout.Row
import Layout.ListGroup
import Layout.ListGroupItem
import Layout.PullRight
import PubSub


type alias Viewer ui uiMsg =
    List ( String, Html (Msg ui uiMsg) ) -> Html (Msg ui uiMsg)


type alias UIList ui =
    List ( String, ui )


type alias Model ui uiMsg =
    { view : Viewer ui uiMsg
    , subscriptions : Subscriptions ui
    , children : UIList ui
    }


type alias Subscriptions ui =
    { insert : Maybe ( String, Decoder ui )
    , update : Maybe ( String, Decoder ui )
    , delete : Maybe String
    }


type Msg ui uiMsg
    = Msg String uiMsg
    | Insert (List ( String, ui ))
    | Update (List ( String, ui ))
    | Delete (List String)


layouts : Dict String (Viewer ui uiMsg)
layouts =
    Dict.fromList
        [ ( "Panel", Layout.Panel.view )
        , ( "Container", Layout.Container.view )
        , ( "Row", Layout.Row.view )
        , ( "ListGroup", Layout.ListGroup.view )
        , ( "ListGroupItem", Layout.ListGroupItem.view )
        , ( "PullRight", Layout.PullRight.view )
        ]


subscriptions : (ui -> Sub uiMsg) -> Model ui uiMsg -> Sub (Msg ui uiMsg)
subscriptions uiSubscriptions model =
    let
        sub ( key, child ) =
            child |> uiSubscriptions |> Sub.map (Msg key)

        childSubs =
            List.map sub model.children

        insertSub =
            case model.subscriptions.insert of
                Just ( insertKey, decoder ) ->
                    [ PubSub.subscribe insertKey <| decodeInsert decoder ]

                Nothing ->
                    []

        updateSub =
            case model.subscriptions.update of
                Just ( updateKey, decoder ) ->
                    [ PubSub.subscribe updateKey <| decodeUpdate decoder ]

                Nothing ->
                    []

        deleteSub =
            case model.subscriptions.delete of
                Just deleteKey ->
                    [ PubSub.subscribe deleteKey decodeDelete ]

                Nothing ->
                    []
    in
        Sub.batch (insertSub ++ updateSub ++ deleteSub ++ childSubs)


decodeModel : String -> Decoder ui -> Decoder (Model ui uiMsg)
decodeModel type_ decodeUI =
    case Dict.get type_ layouts of
        Nothing ->
            Json.fail "not a layout"

        Just view ->
            decodeChildren view decodeUI


decodeChildren : Viewer ui uiMsg -> Decoder ui -> Decoder (Model ui uiMsg)
decodeChildren view decoder =
    Json.map2 (Model view)
        (oneOf
            [ field "subscriptions" (decodeSubscriptions decoder)
            , Json.succeed
                (Subscriptions Nothing Nothing Nothing)
            ]
        )
        (Json.map List.reverse (field "children" (Json.keyValuePairs (lazy (\_ -> decoder)))))


decodeSubscriptions : Decoder ui -> Decoder (Subscriptions ui)
decodeSubscriptions uiDecoder =
    let
        makeSub name =
            ( name, uiDecoder )

        decoder fieldName =
            Json.maybe <| Json.map makeSub <| Json.field fieldName Json.string
    in
        Json.map3 Subscriptions
            (decoder "insert")
            (decoder "update")
            (Json.maybe (Json.field "delete" Json.string))


decodeInsert : Decoder ui -> Decoder (Msg ui uiMsg)
decodeInsert uiDecoder =
    Json.map Insert
        (Json.keyValuePairs uiDecoder)


decodeUpdate : Decoder ui -> Decoder (Msg ui uiMsg)
decodeUpdate uiDecoder =
    Json.map Update
        (Json.keyValuePairs uiDecoder)


decodeDelete : Decoder (Msg ui uiMsg)
decodeDelete =
    Json.map Delete
        (Json.list Json.string)


update :
    (uiMsg -> ui -> ( ui, Cmd uiMsg ))
    -> Msg ui uiMsg
    -> Model ui uiMsg
    -> ( Model ui uiMsg, Cmd (Msg ui uiMsg) )
update updateUI msg model =
    case msg of
        Msg key uiMsg ->
            model.children
                |> updateChildren updateUI key uiMsg
                |> Tuple.mapFirst (\children -> { model | children = children })

        Insert inserts ->
            updateInserts (Debug.log "inserts" inserts) model

        Update updates ->
            model ! []

        Delete deletes ->
            updateDeletes deletes model


updateChildren :
    (uiMsg -> ui -> ( ui, Cmd uiMsg ))
    -> String
    -> uiMsg
    -> UIList ui
    -> ( UIList ui, Cmd (Msg ui uiMsg) )
updateChildren uiUpdate updateKey msg models =
    let
        combine key ( newUI, newCmd ) ( uis, cmd ) =
            ( ( key, newUI ) :: uis
            , Cmd.batch
                [ Cmd.map (Msg key) newCmd
                , cmd
                ]
            )

        update ( key, old ) result =
            let
                current =
                    if key == updateKey then
                        uiUpdate msg old
                    else
                        ( old, Cmd.none )
            in
                combine key current result
    in
        List.foldr update ( [], Cmd.none ) models


updateInserts : List ( String, ui ) -> Model ui msg -> ( Model ui msg, Cmd (Msg ui msg) )
updateInserts inserts model =
    { model | children = inserts ++ model.children } ! []

updateDeletes : List String -> Model ui msg -> ( Model ui msg, Cmd (Msg ui msg) )
updateDeletes deletes model =
    let
        filter (key, _) =
            not (List.member key deletes)
    in
        { model | children = List.filter filter model.children } ! []

view : (ui -> Html msg) -> Model ui msg -> Html (Msg ui msg)
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
