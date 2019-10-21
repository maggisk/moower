module Animation exposing (Animation, Layer, Frame, FrameLayer, Easing,
  new, seek, forward, newFrame, addFrame, updateFrame, deleteFrame, changeFrameOrder,
  newLayer, deleteLayer, updateLayer, changeLayerOrder, findLayer, updateFrameLayer,
  easingEnum, ease,
  draw)

import Canvas exposing (Renderable, texture)
import Canvas.Texture as Texture exposing (Texture)
import Canvas.Settings.Advanced as Adv
import ListExtras exposing (getAt, updateAt, deleteAt, reinsert, zip)
import Point exposing (Point)
import Rect exposing (Rect)


type alias Animation =
  { frames : List Frame
  , layers : List Layer
  , elapsed : Float
  }


type alias Layer =
  { texture : Texture
  , name : String
  , base64 : String
  }


type alias Frame =
  { duration : Float
  , easing : Easing
  , layers : List FrameLayer
  }


type alias FrameLayer =
  { pos : Point
  , scale : Point
  , angle : Float
  , alpha : Float
  , easing : Easing
  , texture : Texture
  }


new : Animation
new =
  Animation [Frame 0.3 Linear []] [] 0.0


seek : Float -> Animation -> Animation
seek time animation =
  { animation | elapsed = time }


forward : Float -> Animation -> Animation
forward time animation =
  { animation | elapsed = animation.elapsed + time }


newFrame : Int -> Animation -> Maybe Animation
newFrame index animation =
  getAt index animation.frames
  |> Maybe.andThen ((addFrame animation) >> Just)


addFrame : Animation -> Frame -> Animation
addFrame animation frame =
  { animation | frames = frame :: animation.frames }


updateFrame : Int -> (Frame -> Frame) -> Animation -> Animation
updateFrame frameIdx transform animation =
  { animation | frames = updateAt frameIdx transform animation.frames }


deleteFrame : Int -> Animation -> Animation
deleteFrame idx animation =
  if List.length animation.frames > 1
  then { animation | frames = deleteAt idx animation.frames }
  else animation


changeFrameOrder : Int -> Int -> Animation -> Animation
changeFrameOrder from to animation =
  { animation | frames = reinsert from to animation.frames }


newLayer : Layer -> Animation -> Animation
newLayer layer animation =
  let
    addFrameLayer frame =
      { frame | layers = (FrameLayer (Point 0.0 0.0) (Point 1.0 1.0) 0.0 1.0 Linear layer.texture) :: frame.layers }
  in
    { animation
    | layers = layer :: animation.layers
    , frames = List.map addFrameLayer animation.frames
    }


deleteLayer : Int -> Animation -> Animation
deleteLayer idx animation =
  { animation
  | layers = deleteAt idx animation.layers
  , frames = List.map (\f -> { f | layers = deleteAt idx f.layers }) animation.frames
  }


updateLayer : Int -> (Layer -> Layer) -> Animation -> Animation
updateLayer layerIdx transform animation =
  { animation | layers = updateAt layerIdx transform animation.layers }


changeLayerOrder : Int -> Int -> Animation -> Animation
changeLayerOrder from to animation =
  let
    changeFrame frame =
      { frame | layers = reinsert from to frame.layers }
  in
    { animation
    | frames = List.map changeFrame animation.frames
    , layers = reinsert from to animation.layers
    }


findLayer : Point -> Int -> Animation -> Maybe Int
findLayer pos frameIdx animation =
  getAt frameIdx animation.frames
  |> Maybe.andThen (\frame ->
    frame.layers
    |> List.indexedMap Tuple.pair
    |> List.reverse
    |> List.filterMap (getIndexIfInside pos)
    |> List.head
  )


getIndexIfInside : Point -> (Int, FrameLayer) -> Maybe Int
getIndexIfInside pos (i, layer) =
  let
    { width, height } = Texture.dimensions layer.texture
    rect = Rect (layer.pos.x - width / 2) (layer.pos.y - height / 2) width height
  in
    if Rect.contains layer.angle pos rect
    then Just i
    else Nothing


updateFrameLayer : Int -> Int -> (FrameLayer -> FrameLayer) -> Animation -> Animation
updateFrameLayer frameIdx layerIdx transform animation =
  let
    updateFrameLayers frame =
      { frame | layers = updateAt layerIdx transform frame.layers }
  in
    { animation | frames = updateAt frameIdx updateFrameLayers animation.frames }


type Easing
  = Linear
  | EaseIn
  | EaseOut

easingEnum = [Linear, EaseIn, EaseOut]

ease : Easing -> Float -> Float
ease easing t =
  case easing of
    Linear ->
      t
    EaseIn ->
      t^2
    EaseOut ->
      1 - (t - 1)^2


draw : Point -> Animation -> List Renderable
draw center animation =
  drawAt center animation.elapsed animation


drawAt : Point -> Float -> Animation -> List Renderable
drawAt center time animation =
  findFrames time animation.frames
  |> Maybe.andThen (\(passed, f1, f2) -> Just (drawExact center passed f1 f2))
  |> Maybe.withDefault []


findFrames : Float -> List Frame -> Maybe (Float, Frame, Frame)
findFrames time frames =
  case frames of
    f1 :: f2 :: rest ->
      if time < f1.duration
      then Just (time, f1, f2)
      else findFrames (time - f1.duration) (f2 :: rest)
    last :: rest ->
      Just (time, last, last)
    _ ->
      Nothing


drawExact : Point -> Float -> Frame -> Frame -> List Renderable
drawExact center passed this next =
  List.map2 (drawExactLayer center (passed / this.duration)) this.layers next.layers


drawExactLayer : Point -> Float -> FrameLayer -> FrameLayer -> Renderable
drawExactLayer center percent this next =
  let
    thisPos = ease this.easing percent
    nextPos = 1 - thisPos

    adjust toVal =
      ((toVal this) * thisPos) + ((toVal next) * nextPos)

    { width, height } = Texture.dimensions this.texture

    x = ((adjust (.pos >> .x)) - width / 2)
    y = ((adjust (.pos >> .y)) - height / 2)

    transforms =
      [ Adv.translate (center.x + x) (center.y + y)
      , Adv.scale (adjust (.scale >> .x)) (adjust (.scale >> .y))
      ]

    settings =
      [ Adv.alpha (adjust .alpha), Adv.transform transforms ]
  in
    texture settings (0.0, 0.0) this.texture
