module Common exposing (dataTestId)

import Html exposing (Attribute)
import Html.Attributes


dataTestId : String -> Attribute msg
dataTestId =
    Html.Attributes.attribute "data-test-id"
