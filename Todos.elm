module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json


type Visibility
    = All
    | Remaining
    | Completed


model =
    { newTodoDescription = ""
    , todos =
        [ { editing = False, description = "get bread", completed = True }
        , { editing = False, description = "pack", completed = False }
        ]
    , visibility = All
    }


type alias TodoId =
    Int


type Msg
    = ToggleCompleted TodoId
    | ToggleEditing TodoId
    | EditingKeyUp TodoId Int
    | UpdateTodoDescription TodoId String
    | RemoveTodo TodoId
    | UpdateNewTodoDescription String
    | NewTodoKeyUp Int
    | ClearCompleted
    | ToggleAllCompleted
    | UpdateVisibility Visibility


removeTodo todos i =
    List.concat
        [ List.take i todos
        , List.drop (i + 1) todos
        ]


updateTodoAtIndex todos i updater =
    let
        todo =
            List.head (List.drop i todos)
    in
        case todo of
            Just todo ->
                List.concat
                    [ List.take i todos
                    , [ (updater todo) ]
                    , List.drop (i + 1) todos
                    ]

            Nothing ->
                todos


toggleCompleted todo =
    { todo | completed = (not todo.completed) }


toggleEditing todo =
    { todo | editing = (not todo.editing) }


updateDescription description todo =
    { todo | description = description }


toggleCompletedAtIndex todos i =
    updateTodoAtIndex todos i toggleCompleted


toggleEditingTodoAtIndex todos i =
    updateTodoAtIndex todos i toggleEditing


updateDescriptionAtIndex todos i description =
    updateTodoAtIndex todos i (updateDescription description)


setCompleted completed todo =
    { todo | completed = completed }


toggleAllCompleted todos =
    let
        completed =
            List.all .completed todos |> not
    in
        List.map (setCompleted completed) todos


update msg model =
    case msg of
        UpdateVisibility visibility ->
            { model | visibility = visibility }

        ToggleAllCompleted ->
            { model | todos = toggleAllCompleted model.todos }

        ToggleCompleted i ->
            { model | todos = (toggleCompletedAtIndex model.todos i) }

        ToggleEditing i ->
            { model | todos = (toggleEditingTodoAtIndex model.todos i) }

        EditingKeyUp i key ->
            if key == 13 then
                { model | todos = (toggleEditingTodoAtIndex model.todos i) }
            else
                model

        UpdateTodoDescription i description ->
            { model | todos = (updateDescriptionAtIndex model.todos i description) }

        RemoveTodo i ->
            { model | todos = (removeTodo model.todos i) }

        UpdateNewTodoDescription description ->
            { model | newTodoDescription = description }

        NewTodoKeyUp key ->
            if key == 13 then
                { model
                    | todos =
                        List.concat
                            [ model.todos
                            , [ { editing = False, description = model.newTodoDescription, completed = False } ]
                            ]
                    , newTodoDescription = ""
                }
            else
                model

        ClearCompleted ->
            { model | todos = (List.filter isRemaining model.todos) }


withStyle html =
    div []
        [ node "style"
            [ attribute "type" "text/css" ]
            [ text "@import url(todos.css)" ]
        , html
        ]


sectionHeader model =
    header
        [ class "header" ]
        [ h1 [] [ text "todos" ]
        , input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , value model.newTodoDescription
            , autofocus True
            , onInput UpdateNewTodoDescription
            , onKeyUp NewTodoKeyUp
            ]
            []
        ]


todoItemClass todo =
    if todo.completed then
        "completed"
    else if todo.editing then
        "editing"
    else
        ""


onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)


todoitem i todo =
    li [ class (todoItemClass todo) ]
        [ div [ class "view", onDoubleClick (ToggleEditing i) ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked todo.completed
                , onClick (ToggleCompleted i)
                ]
                []
            , label [] [ text todo.description ]
            , button [ class "destroy", onClick (RemoveTodo i) ] []
            ]
        , input
            [ class "edit"
            , value todo.description
            , onKeyUp (EditingKeyUp i)
            , onInput (UpdateTodoDescription i)
            ]
            []
        ]


visibleTodos model =
    case model.visibility of
        All ->
            model.todos

        Remaining ->
            List.filter isRemaining model.todos

        Completed ->
            List.filter .completed model.todos


todolist model =
    ul [ class "todo-list" ]
        (List.indexedMap
            todoitem
            (visibleTodos model)
        )


sectionMain model =
    section [ class "main" ]
        [ input [ class "toggle-all", type_ "checkbox" ] []
        , label [ for "toggle-all", onClick ToggleAllCompleted ] [ text "Mark all as complete" ]
        , (todolist model)
        ]


isRemaining todo =
    todo.completed == False


remainingItems model =
    List.filter isRemaining model.todos


remainingCount model =
    List.length (remainingItems model)


itemsLeftLabel count =
    if count == 1 then
        " item left"
    else
        " items left"


classForVisibility v current =
    if v == current then
        "selected"
    else
        ""


sectionFooter model =
    section [ class "footer" ]
        [ span [ class "todo-count" ]
            [ strong [] [ text (toString (remainingCount model)) ]
            , text (itemsLeftLabel (remainingCount model))
            ]
        , ul [ class "filters" ]
            [ li [] [ a [ onClick (UpdateVisibility All), class (classForVisibility All model.visibility) ] [ text "All" ] ]
            , li [] [ a [ onClick (UpdateVisibility Remaining), class (classForVisibility Remaining model.visibility) ] [ text "Active" ] ]
            , li [] [ a [ onClick (UpdateVisibility Completed), class (classForVisibility Completed model.visibility) ] [ text "Completed" ] ]
            ]
        , button [ class "clear-completed", onClick ClearCompleted ] [ text "Clear completed" ]
        ]


view model =
    section
        [ attribute "class" "todoapp"
        ]
        [ (sectionHeader model), (sectionMain model), (sectionFooter model) ]
        |> withStyle


main =
    Html.beginnerProgram { model = model, view = view, update = update }
