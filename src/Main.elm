port module Main exposing (..)

import Browser
import Browser.Events
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- MAIN


main : Program NoteCoords Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    noteCoordsE GotNoteCoords


port colorToolBar : Encode.Value -> Cmd msg


port noteCoordsE : (NoteCoords -> msg) -> Sub msg



-- MODEL


type alias NoteCoords =
    { x : Int, y : Int }


type alias Note =
    { id : Int
    , title : String
    , text : String
    , color : String
    }


type alias Model =
    { isFormOpen : Bool
    , isModalOpen : Bool
    , formTitle : String
    , formText : String
    , modalTitle : String
    , modalText : String
    , modalId : Int
    , showColorToolTip : Bool
    , colorToolTipId : Int
    , noteCoords : NoteCoords
    , notes : List Note
    }


init : NoteCoords -> ( Model, Cmd Msg )
init noteCoords =
    ( { isFormOpen = False
      , isModalOpen = False
      , formTitle = ""
      , formText = ""
      , modalTitle = ""
      , modalText = ""
      , modalId = 0
      , showColorToolTip = False
      , colorToolTipId = 0
      , noteCoords = noteCoords
      , notes = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = OpenForm
    | CloseForm
    | FormTitle String
    | FormText String
    | AddNote
    | CloseAndAddNote
    | DeleteNote Int
    | OpenModal Int
    | CloseModal
    | ModalTitle String
    | ModalText String
    | OpenToolTip Int
    | CloseToolTip
    | GotNoteCoords NoteCoords
    | ChangeColor String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenForm ->
            ( { model | isFormOpen = True }, Cmd.none )

        CloseForm ->
            ( { model | isFormOpen = False }, Cmd.none )

        FormTitle value ->
            ( { model | formTitle = value }, Cmd.none )

        FormText value ->
            ( { model | formText = value }, Cmd.none )

        AddNote ->
            if model.formTitle /= "" || model.formText /= "" then
                addNote model

            else
                ( model, Cmd.none )

        CloseAndAddNote ->
            if model.formTitle /= "" || model.formText /= "" then
                addNote model

            else
                ( { model | isFormOpen = False }, Cmd.none )

        DeleteNote id ->
            let
                newNotes =
                    List.filter (\note -> note.id /= id) model.notes
            in
            ( { model | notes = newNotes }, Cmd.none )

        OpenModal id ->
            let
                { title, text } =
                    case List.filter (\note -> note.id == id) model.notes of
                        note :: _ ->
                            note

                        [] ->
                            { id = id, title = "", text = "", color = "white" }
            in
            ( { model | isModalOpen = True, modalTitle = title, modalText = text, modalId = id }, Cmd.none )

        CloseModal ->
            let
                amendedNotes =
                    List.map
                        (\note ->
                            if note.id == model.modalId then
                                { note | title = model.modalTitle, text = model.modalText }

                            else
                                note
                        )
                        model.notes
            in
            ( { model | isModalOpen = False, notes = amendedNotes }, Cmd.none )

        ModalTitle value ->
            ( { model | modalTitle = value }, Cmd.none )

        ModalText value ->
            ( { model | modalText = value }, Cmd.none )

        OpenToolTip value ->
            ( { model | showColorToolTip = True, colorToolTipId = value }, colorToolBar (Encode.int value) )

        GotNoteCoords value ->
            ( { model | noteCoords = value }, Cmd.none )

        ChangeColor value ->
            let
                amendedNotes =
                    List.map
                        (\note ->
                            if note.id == model.colorToolTipId then
                                { note | color = value }

                            else
                                note
                        )
                        model.notes
            in
            ( { model | notes = amendedNotes }, Cmd.none )

        CloseToolTip ->
            ( { model | showColorToolTip = False, colorToolTipId = 0 }, Cmd.none )



-- UPDATE HELPER FUNCTIONS


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


addNote : Model -> ( Model, Cmd Msg )
addNote model =
    let
        newNotes =
            model.notes ++ [ { id = newId model, title = model.formTitle, text = model.formText, color = "white" } ]
    in
    ( { model | notes = newNotes, formTitle = "", formText = "", isFormOpen = False }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewModal model
        , main_
            [ onClickContains "form" ]
            [ viewHeader
            , viewFormContainer model
            , div []
                [ viewNotes model
                , if List.length model.notes > 0 then
                    div [] []

                  else
                    viewPlaceholder
                ]
            , if model.showColorToolTip then
                viewColorTooltip model

              else
                div [] []
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
        , classList [ ( "form-open", model.isFormOpen ) ]
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
                , onInput FormTitle
                , value model.formTitle
                ]
                []
            , input
                [ id "note-text"
                , placeholder "Take a note..."
                , type_ "text"
                , onInput FormText
                , value model.formText
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
                    , onClickStopProp CloseForm
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
    div [ class "note", style "background" note.color, onClickStopProp (OpenModal note.id) ]
        [ div
            [ classList [ ( "note-title", note.title /= "" ) ]
            ]
            [ text note.title ]
        , div [ class "note-text" ] [ text note.text ]
        , div [ class "toolbar-container" ]
            [ div [ class "toolbar" ]
                [ img
                    [ class "toolbar-color"
                    , src "https://icon.now.sh/palette"
                    , class ("toolbar-color" ++ String.fromInt note.id)
                    , onMouseOver (OpenToolTip note.id)
                    ]
                    []
                , img
                    [ class "toolbar-delete"
                    , src "https://icon.now.sh/delete"
                    , onClickStopProp (DeleteNote note.id)
                    ]
                    []
                ]
            ]
        ]


viewModal : Model -> Html Msg
viewModal model =
    div [ classList [ ( "modal", True ), ( "open-modal", model.isModalOpen ) ] ]
        [ div [ class "modal-content" ]
            [ input
                [ class "modal-title"
                , placeholder "Title"
                , type_ "text"
                , onInput ModalTitle
                , value model.modalTitle
                ]
                []
            , input
                [ class "modal-text"
                , placeholder "Take a note..."
                , type_ "text"
                , onInput ModalText
                , value model.modalText
                ]
                []
            , span [ class "modal-close-button", onClick CloseModal ] [ text "Close" ]
            ]
        ]


viewColorTooltip : Model -> Html Msg
viewColorTooltip model =
    div
        [ id "color-tooltip"
        , style "display" "flex"
        , style "transform"
            ("translate(" ++ String.fromInt model.noteCoords.x ++ "px, " ++ String.fromInt model.noteCoords.y ++ "px)")
        , onMouseLeave CloseToolTip
        , onClickColor ChangeColor
        ]
        [ div [ class "color-option", attribute "data-color" "#fff", id "white" ] []
        , div [ class "color-option", attribute "data-color" "#d7aefb", id "purple" ] []
        , div [ class "color-option", attribute "data-color" "#fbbc04", id "orange" ] []
        , div [ class "color-option", attribute "data-color" "#a7ffeb", id "teal" ] []
        ]



-- HELPER FUNCTIONS


onClickContains : String -> Attribute Msg
onClickContains nodeId =
    on "click" (containsDecoder nodeId)


containsDecoder : String -> Decoder Msg
containsDecoder nodeId =
    Decode.at [ "target" ] (isOutsideForm nodeId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed CloseAndAddNote

                else
                    Decode.succeed OpenForm
            )


isOutsideForm : String -> Decoder Bool
isOutsideForm nodeId =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if id == nodeId then
                        Decode.succeed False

                    else
                        Decode.fail ""
                )
        , Decode.lazy (\_ -> isOutsideForm nodeId |> Decode.field "parentNode")
        , Decode.succeed True
        ]


onClickStopProp : Msg -> Attribute Msg
onClickStopProp message =
    stopPropagationOn "click" (Decode.map alwaysStop (Decode.succeed message))


alwaysStop : a -> ( a, Bool )
alwaysStop x =
    ( x, True )


onClickColor : (String -> Msg) -> Attribute Msg
onClickColor message =
    on "click" (Decode.map message colorDecoder)


colorDecoder : Decoder String
colorDecoder =
    Decode.at [ "target", "dataset", "color" ] Decode.string
