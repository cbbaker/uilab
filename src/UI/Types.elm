module UI.Types exposing (Model(..)
                         , LayoutType
                         , LayoutViewer
                         , Subscriptions
                         , ChoiceType
                         , Msg(..)
                         , LayoutMsgType(..)
                         , ChoiceMsgType(..)
                         )


{-| UI.Types contains the model definitions for the recursive UI
type. The Layout type needs to be in the same file because two Elm
modules cannot import each other.

@docs Model, LayoutType, LayoutViewer, Subscriptions, ChoiceType, Msg, LayoutMsgType, ChoiceMsgType
-}

import Html exposing (..)
import Json.Decode exposing (Decoder)

{-| Recursive data type for UI elements.
-}
type Model pane paneMsg
    = Layout (LayoutType pane paneMsg)
    | Choice (ChoiceType pane paneMsg)
    | Pane pane


{-| A UI element with UI children that displays all its children.
-}
type alias LayoutType pane paneMsg =
    { viewer : LayoutViewer pane paneMsg
    , subscriptions : Subscriptions pane paneMsg
    , children : List ( String, Model pane paneMsg )
    }


{-| A function that renders a layout, given rendered children
-}
type alias LayoutViewer pane paneMsg =
    List ( String, Html (LayoutMsgType pane paneMsg) ) -> Html (LayoutMsgType pane paneMsg)


{-| Subscriptions to update Layouts
-}
type alias Subscriptions pane paneMsg =
    { insert : Maybe ( String, Decoder (Model pane paneMsg) )
    , update : Maybe ( String, Decoder (Model pane paneMsg) )
    , delete : Maybe String
    }

{-| A UI element with UI children that displays one of its children at
a time.
-}
type alias ChoiceType pane paneMsg =
    { subscription : Maybe String
    , current : String
    , children : List ( String, Model pane paneMsg )
    }


{-| UI message type
-}
type Msg pane paneMsg
    = LayoutMsg (LayoutMsgType pane paneMsg)
    | ChoiceMsg (ChoiceMsgType pane paneMsg)
    | PaneMsg paneMsg


{-| Layout message type
-}
type LayoutMsgType pane paneMsg
    = LayoutChildMsg String (Msg pane paneMsg)
    | Insert (List ( String, Model pane paneMsg ))
    | Update (List ( String, Model pane paneMsg ))
    | Delete (List String)

{-| Choice message type
-}
type ChoiceMsgType pane paneMsg
    = ChoiceChildMsg String (Msg pane paneMsg)
    | Choose String
