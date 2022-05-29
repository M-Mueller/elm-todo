port module Main exposing (..)

import Browser
import Html exposing (Html, article, button, div, h1, input, span, text)
import Html.Attributes exposing (class, classList, disabled, style, value)
import Html.Events exposing (onClick, onInput)
import Icons
import Json.Decode
import Json.Encode
import Random
import Task
import Time
import Uuid4



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- PORTS


port putTodo : Json.Encode.Value -> Cmd msg


port fetchTodos : () -> Cmd msg


port onPouchError : (Json.Decode.Value -> msg) -> Sub msg


port onTodosChanged : (Json.Decode.Value -> msg) -> Sub msg



-- MODEL


type alias Todo =
    { id : String
    , rev : String
    , text : String
    , isDone : Bool
    , created : Time.Posix
    }


encodeTodo : Todo -> Json.Encode.Value
encodeTodo todo =
    Json.Encode.object
        [ ( "_id", Json.Encode.string todo.id )
        , ( "_rev", Json.Encode.string todo.rev )
        , ( "text", Json.Encode.string todo.text )
        , ( "isDone", Json.Encode.bool todo.isDone )
        , ( "created", Json.Encode.int (Time.posixToMillis todo.created) )
        ]


decodeTodo : Json.Decode.Decoder Todo
decodeTodo =
    Json.Decode.map5 Todo
        (Json.Decode.field "_id" Json.Decode.string)
        (Json.Decode.field "_rev" Json.Decode.string)
        (Json.Decode.field "text" Json.Decode.string)
        (Json.Decode.field "isDone" Json.Decode.bool)
        (Json.Decode.field "created" Json.Decode.int |> Json.Decode.map Time.millisToPosix)


type alias Model =
    { newTodoText : String
    , todos : List Todo
    , lastError : String
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { newTodoText = ""
      , todos = []
      , lastError = ""
      }
    , fetchTodos ()
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        decodeMsg : Json.Decode.Decoder Msg -> Json.Encode.Value -> Msg
        decodeMsg decoder value =
            case Json.Decode.decodeValue decoder value of
                Ok msg ->
                    msg

                Err err ->
                    ShowError (Json.Decode.errorToString err)
    in
    Sub.batch
        [ onPouchError (decodeMsg (Json.Decode.field "message" Json.Decode.string |> Json.Decode.map ShowError))
        , onTodosChanged (decodeMsg (Json.Decode.list decodeTodo |> Json.Decode.map SetTodos))
        ]



-- UPDATE


type Msg
    = SetNewTodoText String
    | SubmitNewTodo
    | UpdateTodoCreated Todo
    | AddTodo Todo
    | SetTodos (List Todo)
    | ReloadTodos
    | ToggleTodo String
    | ShowError String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNewTodoText text ->
            ( { model | newTodoText = text }, Cmd.none )

        SubmitNewTodo ->
            let
                newTodo uuid =
                    { id = uuid
                    , rev = ""
                    , text = model.newTodoText
                    , isDone = False
                    , created = Time.millisToPosix 0
                    }
            in
            ( model, Random.generate (newTodo >> UpdateTodoCreated) Uuid4.uuid4 )

        UpdateTodoCreated todo ->
            ( model, Time.now |> Task.perform (\time -> AddTodo { todo | created = time }) )

        AddTodo todo ->
            ( { model | newTodoText = "" }, putTodo (encodeTodo todo) )

        SetTodos todos ->
            ( { model | todos = todos }, Cmd.none )

        ReloadTodos ->
            ( model, fetchTodos () )

        ToggleTodo id ->
            let
                updatedTodo : Maybe Todo
                updatedTodo =
                    model.todos
                        |> List.filter (\todo -> todo.id == id)
                        |> List.head
                        |> Maybe.map (\todo -> { todo | isDone = not todo.isDone })
            in
            ( model
            , case updatedTodo of
                Just todo ->
                    putTodo (encodeTodo todo)

                Nothing ->
                    Cmd.none
            )

        ShowError error ->
            ( { model | lastError = error }, Cmd.none )



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
    let
        ( doneTodos, remainingTodos ) =
            model.todos
                |> List.sortBy (.created >> Time.posixToMillis)
                |> List.reverse
                |> List.partition .isDone
    in
    div [ class "container" ]
        [ h1 [] [ text "To-Do List" ]
        , div [ class "danger" ] [ text model.lastError ]
        , div [ class "space-x", class "flex" ]
            [ input [ value model.newTodoText, onInput SetNewTodoText, onPressEnter SubmitNewTodo ] []
            , button
                [ style "width" "min-content"
                , onClick SubmitNewTodo
                , disabled (String.isEmpty model.newTodoText)
                ]
                [ Icons.plus ]
            ]
        , div [] (List.map viewTodo (remainingTodos ++ doneTodos))
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
