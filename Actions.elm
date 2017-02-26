module Actions exposing (..)

import Http
import Json.Decode as Dec exposing (..)
import Json.Encode as Enc exposing (..)
import Dict exposing (Dict)
import PubSub


type alias Model =
    { actions : Dict String Action
    }


type alias Action =
    { links : List Link
    , publishes : List Publish
    }


type alias Link =
    { inProgress : Bool
    , url : String
    , method : String
    , body : Dec.Value
    , success : String
    }


type alias Publish =
    { channel : String
    , payload : Payload
    }


type Payload
    = ModelPayload
    | PlainPayload Dec.Value


type Msg
    = Click String (Maybe Dec.Value)
    | ClickResult String Int (Result Http.Error Dec.Value)


activate : String -> Msg
activate which =
    Click which Nothing


activateWithModel : String -> Dec.Value -> Msg
activateWithModel which model =
    Click which <| Just model


inProgress : String -> Model -> Bool
inProgress key =
    .actions
        >> Dict.get key
        >> Maybe.map .links
        >> Maybe.withDefault []
        >> List.any .inProgress

member : String -> Model -> Bool
member key =
    .actions >> Dict.member key


decodeModel : Decoder Model
decodeModel =
    Dec.map Model
        (Dec.dict decodeActions)


decodeActions : Decoder Action
decodeActions =
    Dec.map2 Action
        (field "links" <| Dec.list decodeLink)
        (field "publishes" <| Dec.list decodePublish)


decodeLink : Decoder Link
decodeLink =
    Dec.map4 (Link False)
        (field "url" Dec.string)
        (field "method" Dec.string)
        (field "body" Dec.value)
        (field "success" Dec.string)


decodePublish : Decoder Publish
decodePublish =
    Dec.map2 Publish
        (field "channel" Dec.string)
        decodePayload


decodePayload : Decoder Payload
decodePayload =
    Dec.oneOf
        [ Dec.map PlainPayload (field "payload" Dec.value)
        , Dec.succeed ModelPayload
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click which payload ->
            updateActions which model payload

        ClickResult which index result ->
            updateClickResult which index model result


updateActions : String -> Model -> Maybe Dec.Value -> ( Model, Cmd Msg )
updateActions which model override =
    let
        update { links, publishes } =
            if List.any .inProgress links then
                model ! []
            else
                let
                    ( newLinks, linkCmd ) =
                        updateLinks which links

                    publishCmd =
                        updatePublishes publishes override

                    action =
                        Action newLinks publishes
                in
                    { model | actions = Dict.insert which action model.actions }
                        ! [ linkCmd
                          , publishCmd
                          ]
    in
        Dict.get which model.actions |> Maybe.map update |> Maybe.withDefault (model ! [])


updateLinks : String -> List Link -> ( List Link, Cmd Msg )
updateLinks which oldLinks =
    let
        rec oldLinks index newLinks newCmds =
            case oldLinks of
                oldLink :: rest ->
                    let
                        ( newLink, newCmd ) =
                            followLink which index oldLink
                    in
                        rec rest (index + 1) (newLink :: newLinks) (newCmd :: newCmds)

                [] ->
                    List.reverse newLinks ! newCmds
    in
        rec oldLinks 0 [] []


followLink : String -> Int -> Link -> ( Link, Cmd Msg )
followLink which index link =
    let
        request =
            Http.request
                { method = link.method
                , headers = []
                , url = link.url
                , body = Http.jsonBody link.body
                , expect = Http.expectJson Dec.value
                , timeout = Nothing
                , withCredentials = False
                }
    in
        { link | inProgress = True } ! [ Http.send (ClickResult which index) request ]


updatePublishes : List Publish -> Maybe Dec.Value -> Cmd Msg
updatePublishes publishes override =
    let
        publish { channel, payload } =
            case (Debug.log "updatePublishes" payload) of
                ModelPayload ->
                    PubSub.publish channel (Maybe.withDefault (Enc.object []) override)

                PlainPayload payload ->
                    PubSub.publish channel payload
    in
        publishes |> List.map publish |> Cmd.batch


updateClickResult : String -> Int -> Model -> Result Http.Error Dec.Value -> ( Model, Cmd Msg )
updateClickResult which index model result =
    let
        update action =
            let
                (newLinks, cmd) =
                    linkResult index result action.links
            in
                { model
                    | actions =
                        Dict.insert
                            which
                            { action | links = newLinks }
                            model.actions
                }
                    ! [ cmd ]
    in
        Dict.get which model.actions |> Maybe.map update |> Maybe.withDefault (model ! [])

maybePublishResponse : Link -> Result Http.Error Dec.Value -> List (Cmd Msg)
maybePublishResponse {success} result =
    case result of
        Ok value ->
            [ PubSub.publish success value ]

        Err error ->
            Debug.log "http error" error |> always []
    

linkResult : Int -> Result Http.Error Dec.Value -> List Link -> (List Link, Cmd Msg)
linkResult index value =
    let
        process i link =
            if i == index then
                ( { link | inProgress = False }, maybePublishResponse link value )
            else
                ( link, [] )
    in
        List.indexedMap process
            >> List.unzip
            >> Tuple.mapSecond (List.concat >> Cmd.batch)
