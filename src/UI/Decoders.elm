module UI.Decoders
    exposing
        ( Environment
        , empty
        , bind
        , lookup
        , decodeLayoutDelete
        )

{-| Decoders for the UI types

@docs decodeLayoutDelete
@docs Environment, empty, bind, lookup
-}

import UI.Types exposing (..)
import Json.Decode as Json exposing (..)


-- import Dict exposing (Dict)


{-| Environment that gets passed to the decoders to lookup variables
-}
type alias Environment =
    List ( String, Value )


{-| Create an empty environment
-}
empty : Environment
empty =
    []


{-| Bind a new variable
-}
bind : String -> Value -> Environment -> Environment
bind key value env =
    ( key, value ) :: env


{-| Look up a variable
-}
lookup : String -> Environment -> Maybe Value
lookup key env =
    case env of
        [] ->
            Nothing

        ( k, v ) :: rest ->
            if k == key then
                Just v
            else
                lookup key rest



-- decodeModel :
--     LayoutRegistry pane paneMsg
--     -> (String -> Environment -> Decoder pane)
--     -> Environment
--     -> Decoder (Model pane paneMsg)
-- decodeModel layouts decodePane env =
--     (field "type" string) |> andThen (decodeRest layouts decodePane env)
-- decodeRest :
--     LayoutRegistry pane paneMsg
--     -> (String -> Environment -> Decoder pane)
--     -> Environment
--     -> String
--     -> Decoder (Model pane paneMsg)
-- decodeRest layouts decodePane env type_ =
--     oneOf
--         [ Json.map Layout (decodeLayout layouts decodePane env type_)
--         , Json.map Choice (decodeChoice layouts decodePane env type_)
--         , Json.map Pane (decodePane type_ env)
--         ]
-- decodeLayout :
--     LayoutRegistry pane paneMsg
--     -> (String -> Environment -> Decoder pane)
--     -> Environment
--     -> String
--     -> Decoder (LayoutType pane paneMsg)
-- decodeLayout layouts decodePane env type_ =
--     case Dict.get type_ layouts of
--         Nothing ->
--             Json.fail "not a layout"
--         Just viewer ->
--             decodeLayoutChildren layouts decodePane viewer env
-- decodeLayoutChildren :
--     LayoutRegistry pane paneMsg
--     -> (String -> Environment -> Decoder pane)
--     -> LayoutViewer pane paneMsg
--     -> Environment
--     -> Decoder (LayoutType pane paneMsg)
-- decodeLayoutChildren layouts decodePane viewer env =
--     Json.map2 (LayoutType viewer)
--         (oneOf
--             [ field "subscriptions" <| decodeLayoutSubscriptions layouts decodePane env
--             , Json.succeed
--                 (Subscriptions Nothing Nothing Nothing)
--             ]
--         )
--         (Json.map List.reverse (field "children" (Json.keyValuePairs (lazy (\_ -> decodeModel layouts decodePane env)))))
-- decodeLayoutSubscriptions :
--     LayoutRegistry pane paneMsg
--     -> (String -> Environment -> Decoder pane)
--     -> Environment
--     -> Decoder (Subscriptions pane paneMsg)
-- decodeLayoutSubscriptions layouts decodePane env =
--     let
--         makeSub name =
--             ( name, decodeModel layouts decodePane env )
--         decoder fieldName =
--             Json.maybe <| Json.map makeSub <| Json.field fieldName Json.string
--     in
--         Json.map3 Subscriptions
--             (decoder "insert")
--             (decoder "update")
--             (Json.maybe (Json.field "delete" Json.string))
-- decodeChoice :
--     LayoutRegistry pane paneMsg
--     -> (String -> Environment -> Decoder pane)
--     -> Environment
--     -> String
--     -> Decoder (ChoiceType pane paneMsg)
-- decodeChoice layouts decodePane env type_ =
--     case type_ of
--         "Choice" ->
--             Json.map3 ChoiceType
--                 (maybe (field "subscription" Json.string))
--                 (field "initial" Json.string)
--                 (field "children" (Json.keyValuePairs (lazy (\_ -> decodeModel layouts decodePane env))))
--         _ ->
--             Json.fail "not a choice"
-- decodeLayoutInsert :
--     LayoutRegistry pane paneMsg
--     -> (String -> Environment -> Decoder pane)
--     -> Decoder (LayoutMsgType pane paneMsg)
-- decodeLayoutInsert layouts decodePane =
--     let
--         model =
--             (field "data" value)
--                 |> andThen
--                     (\data ->
--                         let
--                             env =
--                                 bind "data" data empty
--                         in
--                             (field "template" <| decodeModel layouts decodePane env)
--                     )
--     in
--         Json.map Insert
--             (Json.keyValuePairs model)
-- decodeLayoutUpdate :
--     LayoutRegistry pane paneMsg
--     -> (String -> Environment -> Decoder pane)
--     -> Decoder (LayoutMsgType pane paneMsg)
-- decodeLayoutUpdate layouts decodePane =
--     let
--         model =
--             (field "data" value)
--                 |> andThen
--                     (\data ->
--                         let
--                             env =
--                                 bind "data" data empty
--                         in
--                             (field "template" <| decodeModel layouts decodePane env)
--                     )
--     in
--         Json.map Update
--             (Json.keyValuePairs model)


{-| Parses JSON and returns a layout delete message
-}
decodeLayoutDelete : Decoder (LayoutMsgType pane paneMsg)
decodeLayoutDelete =
    Json.map Delete
        (Json.list Json.string)
