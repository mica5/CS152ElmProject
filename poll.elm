--We're gonna use: Buttons, Radio-Buttons, and Text Fields
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

main =
  Html.beginnerProgram { model = model, view = view, update = update }

type alias Model =
  { question : String
  , choice1 : String
  , choice2 : String
  , choice3 : String
  , choice4 : String
  , answer : Int
  }

--MODEL
model : Model
model =
  Model "Your question will show here." "A1 goes here." "A2 goes here." "A3 goes here." "A4 goes here." 1

--UPDATE
type Msg = 
  Submit 
  | SetQuestion String
  | SetAnswer Int String
  | SetCorrectAnswer Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    Submit -> model
    SetQuestion q -> {model | question = q}
    SetAnswer int ans-> 
      case int of
      1 -> { model | choice1 = ans}
      2 -> { model | choice2 = ans}
      3 -> { model | choice3 = ans}
      4 -> { model | choice4 = ans}
      _ -> { model | choice1 = ans}
    SetCorrectAnswer num -> {model | answer = num}

--VIEW
view : Model -> Html Msg
view model =
  div []
    [ input [placeholder "Enter your question here.", onInput SetQuestion] []
    , question "A1" 1
    , question "A2" 2
    , question "A3" 3
    , question "A4" 4
    , button [ onClick Submit ] [ text "Submit" ]
    , div [] [text model.question]
    , div [] [text model.choice1]
    , div [] [text model.choice2]
    , div [] [text model.choice3]
    , div [] [text model.choice4]
    , div [] [text (toString model.answer)]
    ]

radio : String -> Int -> Html Msg
radio value msg =
  label
    [ style [("padding", "20px")]
    ]
    [ input [ type_ "radio", name "answers", onClick (SetCorrectAnswer msg)] []
    , text value
    ]

question : String -> Int -> Html Msg
question name num =
  fieldset []
        [ radio name (num)
        , input [ placeholder "Enter your answer here.", onInput (SetAnswer num)] []
        ]