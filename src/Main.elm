module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Task
import Dom.Scroll as Scroll
import Json.Decode as Json


init : ( Model, Cmd Msg )
init =
    ( { name = "abab", content = "", messages = [], edited = Nothing, editContent = "" }, Task.perform identity <| Task.succeed MessagesScroll )



-- Main


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- Model


type alias Message =
    { name : String, content : String }


type alias Model =
    { name : String, content : String, messages : List Message, edited : Maybe Int, editContent : String }



-- Message


type Msg
    = NoOp
    | NewName String
    | NewContent String
    | ContentKeyDown Int
    | EditStart Int String
    | NewEdit String
    | EditEnd Int
    | RemoveAt Int
    | MessagesScroll



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ name, content, messages, edited, editContent } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewName name ->
            ( { model | name = name }, Cmd.none )

        NewContent content ->
            ( { model | content = content }, Cmd.none )

        ContentKeyDown key ->
            if key == 13 && String.isEmpty content == False then
                ( { model | messages = { name = name, content = content } :: messages, content = "", edited = Nothing }, Cmd.none )
            else
                ( model, Cmd.none )

        EditStart index content ->
            ( { model | edited = Just index, editContent = content }, Cmd.none )

        NewEdit content ->
            ( { model | editContent = content }, Cmd.none )

        EditEnd key ->
            if key == 13 && String.isEmpty editContent == False then
                case edited of
                    Just index ->
                        ( { model | messages = setAt index { name = name, content = editContent } messages, edited = Nothing }, Cmd.none )

                    Nothing ->
                        Debug.crash "Nothing edited id"
            else
                ( model, Cmd.none )

        RemoveAt index ->
            ( { model | messages = removeAt index messages }, Cmd.none )

        MessagesScroll ->
            ( model, Task.attempt (always NoOp) <| Scroll.toBottom "messages" )


view : Model -> Html Msg
view { name, content, messages, edited, editContent } =
    let
        msgText content index =
            case edited of
                Just eindex ->
                    if index == eindex then
                        input [ class "msg-text", value editContent, onInput NewEdit, onKeyDown EditEnd ] []
                    else
                        p [ class "msg-text", onClick <| EditStart index content ] [ text content ]

                Nothing ->
                    p [ class "msg-text", onClick <| EditStart index content ] [ text content ]

        messageElements =
            List.indexedMap
                (\index msg ->
                    if name == msg.name then
                        li [ class "msg-me" ]
                            [ span [ class "msg-name" ] [ text <| "[" ++ msg.name ++ "]" ]
                            , div [ class "msg-text-container" ]
                                [ span [ class "remove-btn", onClick <| RemoveAt index ] [ text "x" ]
                                , msgText msg.content index
                                ]
                            ]
                    else
                        li []
                            [ span [ class "msg-name" ] [ text <| "[" ++ msg.name ++ "]" ]
                            , p [ class "msg-text" ] [ text msg.content ]
                            ]
                )
                messages
                |> List.reverse
    in
        div [ class "container" ]
            [ ul [ id "messages", class "messages" ] messageElements
            , div [ class "name" ]
                [ input
                    [ placeholder "name"
                    , value name
                    , onInput NewName
                    ]
                    []
                ]
            , div [ class "message" ]
                [ input
                    [ placeholder "new message..."
                    , value content
                    , onKeyDown ContentKeyDown
                    , onInput NewContent
                    ]
                    []
                ]
            ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


removeAt : Int -> List a -> List a
removeAt index list =
    List.take index list ++ List.drop (index + 1) list


setAt : Int -> a -> List a -> List a
setAt index e list =
    List.take index list ++ [ e ] ++ List.drop (index + 1) list
