module OffsetCipher exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Char
import String exposing (slice, length, indexes, uncons, fromChar, map)


-- MODEL


type alias Model =
    { message : String
    , rotation : Int
    }


initialModel =
    { message = "Abjurer."
    , rotation = 13
    }



-- UPDATE


type Msg
    = NoOp
    | UpdateString String
    | UpdateNum String


update : Msg -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        UpdateString contents ->
            { model | message = contents }

        UpdateNum i ->
            { model | rotation = parseInt i }


parseInt : String -> Int
parseInt string =
    case String.toInt string of
        Ok value ->
            value

        Err error ->
            0


alphabet =
    "abcdefghijklmnopqrstuvwxyz"


rotateString : Int -> String -> String
rotateString n message =
    map
        (\ch ->
            if Char.isUpper ch then
                Char.toUpper (rotate n (Char.toLower ch))
            else
                (rotate n) ch
        )
        message


rotate : Int -> Char -> Char
rotate n ch =
    let
        start =
            valueOf ch
    in
        case start of
            -1 ->
                ch

            p ->
                charValue ((p + n) % length alphabet)


charValue : Int -> Char
charValue i =
    case slice i (i + 1) alphabet |> String.toList |> List.head of
        Nothing ->
            '?'

        Just ch ->
            Char.toLower ch


valueOf : Char -> Int
valueOf ch =
    case List.head (indexes (fromChar (Char.toLower ch)) alphabet) of
        Nothing ->
            -1

        Just i ->
            i



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ div [] [ textarea [ cols 40, rows 13, value model.message, onInput UpdateString, autofocus True ] [] ]
        , div [] [ textarea [ cols 40, rows 13, value (rotateString model.rotation model.message) ] [] ]
        , div []
            [ input [ type' "number", onInput UpdateNum, value (toString model.rotation) ]
                []
            ]
        ]



--main : Signal Html


main =
    Html.beginnerProgram { model = initialModel, view = view, update = update }
