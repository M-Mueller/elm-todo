module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (class, disabled, style, value)
import Html.Events exposing (onClick, onInput)
import Icons
import Json.Decode



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



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


init : Model
init =
    { newTodoText = "", todos = [] }



-- UPDATE


type Msg
    = SetNewTodoText String
    | AddTodo


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetNewTodoText text ->
            { model | newTodoText = text }

        AddTodo ->
            let
                newTodo =
                    { id = "123", text = model.newTodoText, isDone = False }
            in
            { model | newTodoText = "", todos = newTodo :: model.todos }



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
            [ input [ value model.newTodoText, onInput SetNewTodoText, onPressEnter AddTodo ] []
            , button
                [ style "width" "min-content"
                , onClick AddTodo
                , disabled (String.isEmpty model.newTodoText)
                ]
                [ Icons.plus ]
            ]
        ]
