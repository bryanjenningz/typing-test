import Html exposing (text, div, input, span)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value, style)
import Html.App exposing (program)
import Array exposing (fromList, toList, length, get)
import Maybe exposing (withDefault)
import Random exposing (generate, int)
import String exposing (endsWith, dropRight, split)
import Time exposing (Time, second)

sentence = "This is a short sentence to type for practice."
words = fromList <| split " " sentence

type Msg
  = UpdateInputWord String
  | Tick Time

main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { inputWord : String
  , wordIndex : Int
  , time : Float
  , runClock : Bool
  , charCount : Int
  }

init = (Model "" 0 0 False 0, Cmd.none)

view model =
  div []
    [ input [ onInput UpdateInputWord, value model.inputWord ] []
    , viewWpm model
    , div [] <| toList <| Array.indexedMap (viewWord model) words
    , viewBar model
    ]

viewBar model =
  div
    [ barContainerStyle ]
    [ div 
      [ barStyle <|
          100 * (toFloat model.wordIndex / toFloat (length words)) ]
      []
    ]

viewWord model index word =
  span
    [ if index == model.wordIndex then
        highlightStyle
      else
        style []
    ]
    [ text (word ++ " ") ]

viewWpm model =
  span []
    [ text <| toString
        (round (if isNaN (wpm model) then
                  0
                else 
                  (wpm model)
               )
        ) ++ " wpm" ]
  
wpm : Model -> Float
wpm model =
  (toFloat model.charCount / (model.time / 60)) / 4

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateInputWord inputWord ->
      if endsWith " " inputWord && 
      (withDefault "" <| get model.wordIndex words) == dropRight 1 inputWord &&
      model.wordIndex < length words then
        ( { model | inputWord = ""
          , wordIndex = model.wordIndex + 1
          , runClock = model.wordIndex + 1 < length words
          , charCount = model.charCount + String.length model.inputWord
          }
        , Cmd.none
        )
      else
        ( { model | inputWord = inputWord
          , runClock = model.wordIndex < length words
          }
        , Cmd.none
        )
        
    Tick time ->
      if model.runClock then
        ( { model | time = model.time + 0.1 }, Cmd.none )
      else
        ( model, Cmd.none )

subscriptions model =
  Time.every (0.1 * second) Tick

highlightStyle =
  style [ ("background-color", "yellow") ]

barContainerStyle =
  style
    [ ("width", "200px")
    , ("height", "20px")
    , ("border", "2px solid black")
    ]

barStyle percentage =
  style
    [ ("width", toString percentage ++ "%")
    , ("height", "20px")
    , ("background-color", "blue")
    ]
