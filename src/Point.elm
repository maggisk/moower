module Point exposing (Point, add, dot, vec, rotate, rotateAround)


type alias Point =
  { x : Float
  , y : Float
   }


add : Point -> Point -> Point
add a b =
  Point (a.x + b.x) (a.y + b.y)


dot : Point -> Point -> Float
dot a b =
  a.x * b.x + a.y * b.y


vec : Point -> Point -> Point
vec a b =
  Point (b.x - a.x) (b.y - a.y)


rotate : Float -> Point -> Point
rotate rad point =
  Point (point.x * cos(rad) - point.y * sin(rad))
        (point.x * sin(rad) - point.y * cos(rad))


rotateAround : Float -> Point -> Point -> Point
rotateAround rad center point =
  vec point center
  |> rotate rad
  |> add center
