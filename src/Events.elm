module Events exposing (MouseEvent, mouseEventDecoder)

import Json.Decode as Decode
import Point exposing (Point)


type alias MouseEvent =
  { button : Int
  , buttons : Int
  , movement : Point
  , client: Point
  , altKey : Bool
  , ctrlKey : Bool
  , metaKey : Bool
  }


mouseEventDecoder : Decode.Decoder MouseEvent
mouseEventDecoder =
  Decode.map7 MouseEvent
    (Decode.field "button" Decode.int)
    (Decode.field "buttons" Decode.int)
    (Decode.at [] (decodePoint "movementX" "movementY"))
    (Decode.at [] (decodePoint "clientX" "clientY"))
    (Decode.field "altKey" Decode.bool)
    (Decode.field "ctrlKey" Decode.bool)
    (Decode.field "metaKey" Decode.bool)


decodePoint : String -> String -> Decode.Decoder Point
decodePoint x y =
  Decode.map2 Point
    (Decode.field x Decode.float)
    (Decode.field y Decode.float)
