module Main exposing (..)

import Browser
import Browser.Events
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MODEL


type alias Note =
    { id : Int
    , title : String
    , text : String
    , color : String
    }


type alias Model =
    { isFormOpen : Bool
    , title : String
    , text : String
    , notes : List Note
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { isFormOpen = False
      , title = ""
      , text = ""
      , notes = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Open
    | Close
    | Title String
    | Text String
    | AddNote
    | CloseAndAddNote
    | DeleteNote Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Open ->
            ( { model | isFormOpen = True }, Cmd.none )

        Close ->
            ( { model | isFormOpen = False }, Cmd.none )

        Title value ->
            ( { model | title = value }, Cmd.none )

        Text value ->
            ( { model | text = value }, Cmd.none )

        AddNote ->
            if model.title /= "" || model.text /= "" then
                let
                    newNotes =
                        model.notes ++ [ { id = newId model, title = model.title, text = model.text, color = "white" } ]
                in
                ( { model | notes = newNotes, title = "", text = "", isFormOpen = False }, Cmd.none )

            else
                ( model, Cmd.none )

        CloseAndAddNote ->
            if model.title /= "" || model.text /= "" then
                let
                    newNotes =
                        model.notes ++ [ { id = newId model, title = model.title, text = model.text, color = "white" } ]
                in
                ( { model | notes = newNotes, title = "", text = "", isFormOpen = False }, Cmd.none )

            else
                ( { model | isFormOpen = False }, Cmd.none )

        DeleteNote id ->
            let
                _ =
                    Debug.log "id" id

                newNotes =
                    List.filter (\note -> note.id /= id) model.notes
            in
            ( { model | notes = newNotes }, Cmd.none )


newId : Model -> Int
newId model =
    if List.length model.notes == 0 then
        1

    else
        List.foldl
            (\note acc ->
                if note.id > acc then
                    note.id

                else
                    acc
            )
            0
            model.notes
            + 1



-- VIEW


view : Model -> Html Msg
view model =
    main_ [ onCustomClick ]
        [ viewHeader
        , viewFormContainer model
        , div []
            [ viewNotes model
            , viewPlaceholder
            ]
        ]


viewHeader : Html Msg
viewHeader =
    header []
        [ img
            [ class "header-logo"
            , src
                "https://www.gstatic.com/images/branding/product/1x/keep_48dp.png"
            ]
            []
        , h2 [ class "header-title" ] [ text "Keep" ]
        ]


viewPlaceholder : Html Msg
viewPlaceholder =
    div [ id "placeholder" ]
        [ img [ id "placeholder-logo", src "https://icon.now.sh/lightbulb_outline" ] []
        , p [ id "placeholder-text" ] [ text "Notes you add appear here" ]
        ]


viewFormContainer : Model -> Html Msg
viewFormContainer model =
    div
        [ id "form-container"
        , class
            (if model.isFormOpen then
                "form-open"

             else
                ""
            )
        ]
        [ Html.form [ id "form", autocomplete False ]
            [ input
                [ id "note-title"
                , placeholder "Title"
                , type_ "text"
                , style "display"
                    (if model.isFormOpen then
                        "block"

                     else
                        "none"
                    )
                , onInput Title
                , value model.title
                ]
                []
            , input
                [ id "note-text"
                , placeholder "Take a note..."
                , type_ "text"
                , onInput Text
                , value model.text
                ]
                []
            , div
                [ id "form-buttons"
                , style "display"
                    (if model.isFormOpen then
                        "block"

                     else
                        "none"
                    )
                ]
                [ button
                    [ type_ "button"
                    , id "add-button"
                    , onClickStopProp AddNote
                    ]
                    [ text "Add" ]
                , button
                    [ type_ "button"
                    , id "form-close-button"
                    , onClickStopProp Close
                    ]
                    [ text "Close" ]
                ]
            ]
        ]


viewNotes : Model -> Html Msg
viewNotes model =
    div [ id "notes" ] (List.map viewNote model.notes)


viewNote : Note -> Html Msg
viewNote note =
    div [ class "note", style "background" note.color ]
        [ div
            [ class
                (if note.title == "" then
                    ""

                 else
                    "note-title"
                )
            ]
            [ text note.title ]
        , div [ class "note-text" ] [ text note.text ]
        , div [ class "toolbar-container" ]
            [ div [ class "toolbar" ]
                [ img [ class "toolbar-color", src "https://icon.now.sh/palette" ] []
                , img [ class "toolbar-delete", src "https://icon.now.sh/delete", onClickStopProp (DeleteNote note.id) ] []
                ]
            ]
        ]


onCustomClick : Attribute Msg
onCustomClick =
    on "click" targetDecoder


targetDecoder : Decode.Decoder Msg
targetDecoder =
    Decode.at [ "target" ] isOutsideForm
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed CloseAndAddNote

                else
                    Decode.succeed Open
            )


isOutsideForm : Decode.Decoder Bool
isOutsideForm =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if id == "form" then
                        Decode.succeed False

                    else
                        Decode.fail ""
                )
        , Decode.lazy (\_ -> isOutsideForm |> Decode.field "parentNode")
        , Decode.succeed True
        ]


onClickStopProp : Msg -> Attribute Msg
onClickStopProp message =
    stopPropagationOn "click" (Decode.map alwaysStop (Decode.succeed message))


alwaysStop : a -> ( a, Bool )
alwaysStop x =
    ( x, True )



-- checkParentNode : String -> Decode.Decoder Msg
-- checkParentNode id =
--     case id of
--         "note-title" ->
--             let
--                 _ =
--                     Debug.log "clicked" id
--             in
--             Decode.succeed Open
--         "form-buttons" ->
--             Decode.succeed Open
--         _ ->
--             let
--                 _ =
--                     Debug.log "clicked" id
--             in
--             Decode.succeed Open
