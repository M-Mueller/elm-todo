module Main exposing (..)

import Browser
import Html exposing (Html, article, button, div, h1, input, span, text)
import Html.Attributes exposing (class, classList, disabled, style, value)
import Html.Events exposing (onClick, onInput)
import Icons
import Json.Decode
import Random
import Uuid4



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type alias Todo =
    { id : String
    , text : String
    , isDone : Bool
    }


type alias Model =
    { newTodoText : String
    , todos : List Todo
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { newTodoText = "", todos = [] }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = SetNewTodoText String
    | SubmitNewTodo
    | AddTodo Todo
    | ToggleTodo String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNewTodoText text ->
            ( { model | newTodoText = text }, Cmd.none )

        SubmitNewTodo ->
            let
                newTodo uuid =
                    { id = uuid, text = model.newTodoText, isDone = False }
            in
            ( model, Random.generate (newTodo >> AddTodo) Uuid4.uuid4 )

        AddTodo todo ->
            ( { model | newTodoText = "", todos = todo :: model.todos }, Cmd.none )

        ToggleTodo id ->
            let
                updateTodo : Todo -> Todo
                updateTodo todo =
                    if todo.id == id then
                        { todo | isDone = not todo.isDone }

                    else
                        todo
            in
            ( { model | todos = List.map updateTodo model.todos }, Cmd.none )



-- VIEW


onPressEnter : msg -> Html.Attribute msg
onPressEnter message =
    Html.Events.on "keydown"
        (Json.Decode.field "key" Json.Decode.string
            |> Json.Decode.andThen
                (\key ->
                    if key == "Enter" then
                        Json.Decode.succeed message

                    else
                        Json.Decode.fail "wrong key"
                )
        )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "To-Do List" ]
        , div [ class "space-x", class "flex" ]
            [ input [ value model.newTodoText, onInput SetNewTodoText, onPressEnter SubmitNewTodo ] []
            , button
                [ style "width" "min-content"
                , onClick SubmitNewTodo
                , disabled (String.isEmpty model.newTodoText)
                ]
                [ Icons.plus ]
            ]
        , div [] (List.map viewTodo model.todos)
        ]


viewTodo : Todo -> Html Msg
viewTodo todo =
    article [ class "flex, space-x", classList [ ( "dimmed", todo.isDone ) ] ]
        [ span
            [ class "grow self-center word-break"
            , classList [ ( "line-through", todo.isDone ) ]
            , onClick (ToggleTodo todo.id)
            ]
            [ text todo.text ]
        ]
