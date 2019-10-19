module Animation exposing (Animation, Layer, Frame, FrameLayer, Easing,
  new, seek, forward, newFrame, addFrame, updateFrame, deleteFrame,
  newLayer, deleteLayer, updateLayer, findLayer, updateFrameLayer,
  easingEnum, ease)

import Canvas.Texture as Texture exposing (Texture)
import ListExtras exposing (getAt, updateAt, deleteAt)
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
  { x : Float
  , y : Float
  , angle : Float
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


newLayer : Layer -> Animation -> Animation
newLayer layer animation =
  let
    addFrameLayer frame =
      { frame | layers = (FrameLayer 0.0 0.0 0.0 Linear layer.texture) :: frame.layers }
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
    rect = Rect (layer.x - width / 2) (layer.y - height / 2) width height
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
