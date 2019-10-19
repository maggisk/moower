module Rect exposing (Rect, corners, center, contains)

import Point exposing (Point, vec, dot, rotateAround)


type alias Rect =
  { x : Float
  , y : Float
  , width : Float
  , height : Float
  }


corners : Float -> Rect -> (Point, Point, Point)
corners angle r =
  let
    c = center r
  in
    ( Point r.x r.y |> rotateAround angle c
    , Point (r.x + r.width) r.y |> rotateAround angle c
    -- , Point (r.x + r.width) (r.y + r.height) |> rotateAround angle c
    , Point r.x (r.y + r.height) |> rotateAround angle c
    )


center : Rect -> Point
center rect =
  Point (rect.x + rect.width / 2) (rect.y + rect.height / 2)


contains : Float -> Point -> Rect -> Bool
contains angle m r =
  let
    (a, b, d) = corners angle r
    am = vec a m
    ab = vec a b
    ad = vec a d
  in
    0 < (dot am ab) && (dot am ab) < (dot ab ab) &&
    0 < (dot am ad) && (dot am ad) < (dot ad ad)
