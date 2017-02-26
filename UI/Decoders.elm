module UI.Decoders exposing (..)

import UI.Types exposing (..)
import Json.Decode as Json exposing (..)
import Dict exposing (Dict)

decodeModel : Dict String (LayoutViewer pane paneMsg) -> (String -> Decoder pane) -> Decoder (Model pane paneMsg)
decodeModel layouts decodePane =
    (field "type" string) |> andThen (decodeRest layouts decodePane)


decodeRest : Dict String (LayoutViewer pane paneMsg) -> (String -> Decoder pane) -> String -> Decoder (Model pane paneMsg)
decodeRest layouts decodePane type_ =
    oneOf
        [ Json.map Layout (decodeLayout layouts decodePane type_)
        , Json.map Choice (decodeChoice layouts decodePane type_)
        , Json.map Pane (decodePane type_)
        ]


decodeLayout :
    Dict String (LayoutViewer pane paneMsg)
    -> (String -> Decoder pane)
    -> String
    -> Decoder (LayoutType pane paneMsg)
decodeLayout layouts decodePane type_ =
    case Dict.get type_ layouts of
        Nothing ->
            Json.fail "not a layout"

        Just viewer ->
            decodeLayoutChildren layouts decodePane viewer


decodeLayoutChildren :
    Dict String (LayoutViewer pane paneMsg)
    -> (String -> Decoder pane)
    -> LayoutViewer pane paneMsg
    -> Decoder (LayoutType pane paneMsg)
decodeLayoutChildren layouts decodePane viewer =
    Json.map2 (LayoutType viewer)
        (oneOf
            [ field "subscriptions" <| decodeLayoutSubscriptions layouts decodePane
            , Json.succeed
                (Subscriptions Nothing Nothing Nothing)
            ]
        )
        (Json.map List.reverse (field "children" (Json.keyValuePairs (lazy (\_ -> decodeModel layouts decodePane)))))


decodeLayoutSubscriptions :
    Dict String (LayoutViewer pane paneMsg)
    -> (String -> Decoder pane)
    -> Decoder (Subscriptions pane paneMsg)
decodeLayoutSubscriptions layouts decodePane =
    let
        makeSub name =
            ( name, decodeModel layouts decodePane )

        decoder fieldName =
            Json.maybe <| Json.map makeSub <| Json.field fieldName Json.string
    in
        Json.map3 Subscriptions
            (decoder "insert")
            (decoder "update")
            (Json.maybe (Json.field "delete" Json.string))


decodeChoice :
    Dict String (LayoutViewer pane paneMsg)
    -> (String -> Decoder pane)
    -> String
    -> Decoder (ChoiceType pane paneMsg)
decodeChoice layouts decodePane type_ =
    case type_ of
        "Choice" ->
            Json.map3 ChoiceType
                (maybe (field "subscription" Json.string))
                (field "initial" Json.string)
                (field "children" (Json.keyValuePairs (lazy (\_ -> decodeModel layouts decodePane))))

        _ ->
            Json.fail "not a choice"


decodeLayoutInsert :
    Dict String (LayoutViewer pane paneMsg)
    -> (String -> Decoder pane)
    -> Decoder (LayoutMsgType pane paneMsg)
decodeLayoutInsert layouts decodePane =
    Json.map Insert
        (Json.keyValuePairs <| decodeModel layouts decodePane)


decodeLayoutUpdate :
    Dict String (LayoutViewer pane paneMsg)
    -> (String -> Decoder pane)
    -> Decoder (LayoutMsgType pane paneMsg)
decodeLayoutUpdate layouts decodePane =
    Json.map Update
        (Json.keyValuePairs <| decodeModel layouts decodePane)


decodeLayoutDelete : Decoder (LayoutMsgType pane paneMsg)
decodeLayoutDelete =
    Json.map Delete
        (Json.list Json.string)
