module Orderable exposing (State, Msg(..), Direction(..), update, subscriptions,
  onMouseDown, preventTextHighlight, containerStyle, draggableStyle, draggableAttributes)


-- TODO: detect size of draggable element so it doesn't have to be hardcoded in code


import Browser.Events exposing (onMouseMove, onMouseUp)
import Html exposing (Attribute)
import Html.Events exposing (on, preventDefaultOn)
import Html.Attributes exposing (style)
import Task
import Json.Decode as Decode
import Point exposing (Point)
import Events exposing (MouseEvent, mouseEventDecoder)


type alias State orderable =
  { idx : Int
  , size : Int
  , which : orderable
  , direction : Direction
  , mouseStart : Point
  , mouseAt : Point
  }


type Direction
  = Horizontal
  | Vertical


type Msg orderable
  = Start Int Int orderable Direction MouseEvent
  | Move MouseEvent
  | End MouseEvent
  | Apply (State orderable) Int
  | NoOp


update : ((Msg orderable) -> msg) -> (Msg orderable) -> Maybe (State orderable) -> (Maybe (State orderable), Cmd msg)
update toMsg msg maybeState =
  case (maybeState, msg) of
    (Nothing, Start index size which direction event) ->
      ( Just (State index size which direction event.client event.client)
      , Cmd.none
      )

    (Just state, Move event) ->
      ( Just { state | mouseAt = event.client }
      , Cmd.none
      )

    (Just state, End event) ->
      ( Nothing
      , Task.perform identity (Task.succeed (toMsg (Apply state (state.idx + ((dragDistance state) // state.size)))))
      )

    _ ->
      ( Nothing, Cmd.none )


subscriptions : (Maybe (State orderable)) -> ((Msg orderable) -> msg) -> Sub msg
subscriptions state toMsg =
  case state of
    Just _ ->
      Sub.batch
        [ onMouseMove (Decode.map (\e -> toMsg (Move e)) mouseEventDecoder)
        , onMouseUp (Decode.map (\e -> toMsg (End e)) mouseEventDecoder)
        ]
    Nothing ->
      Sub.none


containerStyle : Attribute msg
containerStyle =
  style "transform-style" "preserve-3d"


draggableAttributes : Int -> Int -> orderable -> Direction -> (Msg orderable -> msg) -> Maybe (State orderable) -> List (Attribute msg)
draggableAttributes idx size which direction toMsg state =
  [ draggableStyle idx which state
  , onMouseDown idx size which direction toMsg
  , preventTextHighlight toMsg
  ]


onMouseDown : Int -> Int -> orderable -> Direction -> ((Msg orderable) -> msg) -> Attribute msg
onMouseDown index size which direction toMsg =
  on "mousedown" (Decode.map (\e -> toMsg (Start index size which direction e)) mouseEventDecoder)


preventTextHighlight : ((Msg orderable) -> msg) -> Attribute msg
preventTextHighlight toMsg =
  preventDefaultOn "selectstart" (Decode.succeed (toMsg NoOp, True))


translate : Direction -> Int -> String
translate dir px =
  let
    func =
      case dir of
        Horizontal -> "translateX"
        Vertical   -> "translateY"
  in
    String.concat [ func, "(", String.fromInt px, "px)" ]


draggableStyle : Int -> orderable -> (Maybe (State orderable)) -> Attribute msg
draggableStyle idx which maybeState =
  let
    checkType state =
      if which == state.which
      then Just state
      else Nothing

    transform state =
      let
        pixels = dragDistance state
        units = pixels // state.size
      in
        if state.idx == idx then
          Just ((translate state.direction pixels) ++ " translateZ(10px)")
        else if idx < state.idx && state.idx + units <= idx then
          Just (translate state.direction state.size)
        else if idx > state.idx && state.idx + units >= idx then
          Just (translate state.direction -state.size)
        else
          Nothing

    value =
      maybeState
      |> Maybe.andThen checkType
      |> Maybe.andThen transform
      |> Maybe.withDefault "none"
  in
    style "transform" value


dragDistance : State orderable -> Int
dragDistance { direction, mouseAt, mouseStart } =
  case direction of
    Horizontal ->
      round (mouseAt.x - mouseStart.x)
    Vertical ->
      round (mouseAt.y - mouseStart.y)
