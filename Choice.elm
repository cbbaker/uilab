module Choice exposing (..)

import Html exposing (..)
import Json.Decode as Json exposing (..)
import PubSub


type alias UIList ui =
    List ( String, ui )


type alias Model ui =
    { subscription : Maybe String
    , current : String
    , children : UIList ui
    }


type Msg uiMsg
    = Msg String uiMsg
    | Choose String


currentChild : Model ui -> Maybe ui
currentChild { current, children } =
    let
        find list =
            case list of
                ( key, ui ) :: rest ->
                    if key == current then
                        Just ui
                    else
                        find rest

                _ ->
                    Nothing
    in
        find children


subscriptions : (ui -> Sub uiMsg) -> Model ui -> Sub (Msg uiMsg)
subscriptions uiSubscriptions model =
    let
        childSub (key, child) =
            child |> uiSubscriptions |> Sub.map (Msg key)

        chooseSub =
            case model.subscription of
                Just key ->
                    [ PubSub.subscribe key (Json.map Choose Json.string) ]

                Nothing ->
                    []
    in
        Sub.batch (chooseSub ++ (List.map childSub model.children))


decodeModel : String -> Decoder ui -> Decoder (Model ui)
decodeModel type_ decodeUI =
    case type_ of
        "Choice" ->
            Json.map3 Model
                (maybe (field "subscription" Json.string))
                (field "initial" Json.string)
                (field "children" (Json.keyValuePairs (lazy (\_ -> decodeUI))))

        _ ->
            Json.fail "not a choice"


update :
    (uiMsg -> ui -> ( ui, Cmd uiMsg ))
    -> Msg uiMsg
    -> Model ui
    -> ( Model ui, Cmd (Msg uiMsg) )
update updateUI msg model =
    case (Debug.log "Choice update" msg) of
        Msg key uiMsg ->
            model.children
                |> updateChildren updateUI key uiMsg
                |> Tuple.mapFirst (\children -> { model | children = children })

        Choose key ->
            { model | current = key } ! []


updateChildren :
    (uiMsg -> ui -> ( ui, Cmd uiMsg ))
    -> String
    -> uiMsg
    -> UIList ui
    -> ( UIList ui, Cmd (Msg uiMsg) )
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


view : (ui -> Html msg) -> Model ui -> Html (Msg msg)
view viewUI model =
    case currentChild model of
        Just child ->
            child |> viewUI |> Html.map (Msg model.current)

        Nothing ->
            div [] []
