effect module PubSub where { command = MyCmd, subscription = MySub } exposing (publish, subscribe)

{-| Generalized routing messages between components

# PubSub
@docs publish, subscribe

-}

import Dict exposing (Dict)
import Task exposing (Task)
import Json.Decode as Json


-- COMMANDS


type MyCmd msg
    = Publish String Json.Value


{-| Send a message to a particular channel. You might say something like this:

    publish "itemList" Json.Encode.object "delete" (Json.Encode.int itemId)
-}
publish : String -> Json.Value -> Cmd msg
publish channel message =
    command (Publish channel message)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap _ (Publish channel message) =
    Publish channel message



-- SUBSCRIPTIONS


type MySub msg
    = Subscribe String (Json.Decoder msg)


{-| Subscribe to any published messages on a channel. You might say something
like this:

    type Msg = Echo Json.Value | ...

    subscriptions model =
      subscribe "itemList" Echo

-}
subscribe : String -> Json.Decoder msg -> Sub msg
subscribe channel tagger =
    subscription (Subscribe channel tagger)


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
    case sub of
        Subscribe channel tagger ->
            Subscribe channel (Json.map func tagger)



-- MANAGER


type alias State msg =
    SubsDict msg


type alias SubsDict msg =
    Dict String (List (Json.Decoder msg))


init : Task Never (State msg)
init =
    Task.succeed (Debug.log "init" Dict.empty)


type Msg
    = Msg



-- HANDLE APP MESSAGES


(&>) : Task x a -> Task x b -> Task x b
(&>) t1 t2 =
    Task.andThen (\_ -> t2) t1


onEffects :
    Platform.Router msg Msg
    -> List (MyCmd msg)
    -> List (MySub msg)
    -> State msg
    -> Task Never (State msg)
onEffects router cmds subs _ =
    let
        subsDict =
            buildSubDict (Debug.log "onEffects: subs" subs) Dict.empty

        processCmds cmds =
            case cmds of
                [] ->
                    Task.succeed subsDict

                (Publish name msg) :: rest ->
                    let
                        sends =
                            Dict.get name subsDict
                                |> Maybe.withDefault []
                                |> createSends router msg
                    in
                        (Task.sequence sends &> processCmds rest) |> Debug.log "tasks"
    in
        processCmds (Debug.log "onEffects: cmds" cmds)


createSends :
    Platform.Router msg Msg
    -> Json.Value
    -> List (Json.Decoder msg)
    -> List (Platform.Task x ())
createSends router value decoders =
    case decoders of
        decoder :: rest ->
            case Json.decodeValue decoder value of
                Ok msg ->
                    Platform.sendToApp router msg :: createSends router value rest

                Err err ->
                    (Debug.log "decoder error" err) |> (\_ -> createSends router value rest)

        _ ->
            []


buildSubDict : List (MySub msg) -> SubsDict msg -> SubsDict msg
buildSubDict subs dict =
    case subs of
        [] ->
            dict

        (Subscribe name tagger) :: rest ->
            buildSubDict rest (Dict.update name (add tagger) dict)


add : a -> Maybe (List a) -> Maybe (List a)
add value maybeList =
    case maybeList of
        Nothing ->
            Just [ value ]

        Just list ->
            Just (value :: list)


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router msg state =
    case (Debug.log "onSelfMsg msg" msg) of
        _ ->
            Task.succeed (Debug.log "onSelfMsg state" state)
