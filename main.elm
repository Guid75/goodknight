import Debug
import Html exposing (div, button, text)
import Graphics.Element exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Text
import StartApp.Simple as StartApp
import Cards exposing (..)

main =
  StartApp.start { model = model, view = view, update = update }

model = 0

view address model =
  div []
    (List.map (textToDiv landscapeStyle) initialLandscapeDeck)

-- transform anything into an HTML div with just its textual representation
textToDiv : Html.Attribute -> a -> Html.Html
textToDiv attr a = div [attr] [(text << toString) a]

landscapeStyle : Html.Attribute
landscapeStyle =
  style
  [ ("font-size", "15px")
  , ("font-family", "monospace")
  , ("text-align", "center")
  ]

type Action = Increment | Decrement

update action model =
  case action of
    Increment -> model + 1
    Decrement -> model - 1
