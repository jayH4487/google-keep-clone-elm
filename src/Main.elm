module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)



-- MAIN


main =
    view



-- VIEW


view =
    main_ []
        [ header []
            [ img
                [ class "header-logo"
                , src
                    "https://www.gstatic.com/images/branding/product/1x/keep_48dp.png"
                ]
                []
            , h2 [ class "header-title" ] [ text "Keep" ]
            ]
        , div [ id "form-container" ]
            [ Html.form [ id "form", autocomplete False ]
                [ input [ id "note-title", placeholder "Title", type_ "text" ] []
                , input [ id "note-text", placeholder "Take a note...", type_ "text" ] []
                , div [ id "form-buttons" ]
                    [ button [ type_ "submit", id "submit-button" ] [ text "Submit" ]
                    , button [ type_ "button", id "form-close-button" ] [ text "Close" ]
                    ]
                ]
            ]
        , div []
            [ div [ id "notes" ] []
            , div [ id "placeholder" ]
                [ img [ id "placeholder-logo", src "https://icon.now.sh/lightbulb_outline" ] []
                , p [ id "placeholder-text" ] [ text "Notes you add appear here" ]
                ]
            ]
        ]
