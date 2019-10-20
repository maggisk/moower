import Browser
import Browser.Events exposing (onResize, onAnimationFrameDelta, onMouseMove, onMouseUp)
import Browser.Dom exposing (Element, Error, getElement)
import Html exposing (..)
import Html.Attributes exposing (id, class, classList, style, hidden, width, height)
import Html.Events exposing (on, onClick, preventDefaultOn, custom)
import Html.Lazy exposing (lazy)
import File exposing (File)
import Json.Decode as Decode
import Task
import Canvas
import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (transform, translate, rotate)
import Canvas.Texture as Texture exposing (Texture)
import Maybe exposing (andThen, withDefault)
import Debug exposing (log, toString)
import Color
import ListExtras exposing (getAt, insertAt, updateAt, deleteAt)
import Point exposing (Point)
import Rect exposing (Rect)
import Animation exposing (Animation, Layer, Frame, FrameLayer, Easing,
  new, seek, forward, newFrame, addFrame, updateFrame, deleteFrame,
  newLayer, deleteLayer, updateLayer, findLayer, updateFrameLayer,
  easingEnum, ease)


-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias MouseEvent =
  { button : Int
  , buttons : Int
  , movement : Point
  , client: Point
  , altKey : Bool
  , ctrlKey : Bool
  , metaKey : Bool
  }


type alias LayerLoading =
  { name : String
  , base64 : String
  }


type Draggable
  = DragLayer
  | DragFrame


type Direction
  = Horizontal
  | Vertical


type alias Drag =
  { idx : Int
  , which : Draggable
  , direction : Direction
  , mouseStart : Point
  , mouseAt : Point
  , size : Int
  }


type alias Model =
  { dropping : Bool
  , playing : Bool
  , canvas : Rect
  , queued : List LayerLoading
  , animation : Animation
  , currentFrameIdx : Int
  , currentLayerIdx : Int
  , drag : Maybe Drag
  , error : Maybe String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model
      False -- dropping
      False -- playing
      (Rect 0.0 0.0 0.0 0.0) -- canvas
      [] -- queued
      Animation.new -- animation
      0 -- currentFrameIdx
      0 -- currentLayerIdx
      Nothing -- drag
      Nothing -- error
  , Task.attempt CanvasRect (getElement "animation")
  )



-- UPDATE


type Msg
  = NoOp
  | AnimationFrame Float
  | OnDragEnter
  | OnDragLeave
  | FilesDropped File (List File)
  | FilesReady (List String) (List String)
  | TextureLoaded LayerLoading (Maybe Texture)
  | SelectLayer Int
  | DeleteLayer Int
  | NewFrame
  | SelectFrame Int
  | DeleteFrame Int
  | TogglePlay
  | OnWindowResize Int Int
  | CanvasRect (Result Error Element)
  | CanvasMouseDown MouseEvent
  | CanvasMouseMoved MouseEvent
  | DragStart Int Int Draggable Direction MouseEvent
  | DragMove MouseEvent
  | DragEnd MouseEvent


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )

    AnimationFrame delta ->
      ( { model | animation = forward delta model.animation }
      , Cmd.none
      )

    OnDragEnter ->
      ( { model | dropping = True }
      , Cmd.none
      )

    OnDragLeave ->
      ( {model | dropping = False}
      , Cmd.none
      )

    FilesDropped f fs ->
      let
        isImage file = String.startsWith "image/" (File.mime file)
        files = List.filter isImage (f :: fs)
      in
      -- 1) files were dropped onto the canvas area so we read the image as base64 encoded strings
      --    asynchronously because... thats how browsers work
      ( { model | dropping = False }
      , Task.perform
          (FilesReady (List.map File.name files))
          (Task.sequence (List.map File.toUrl files))
      )

    FilesReady names base64s ->
      -- 2) files have been read as base64 encoded strings - now we must render them to create textures
      -- for the canvas - again, asynchronously, because that's how elm-canvas works
      ( { model | queued = model.queued ++ (List.map2 LayerLoading names base64s) }
      , Cmd.none
      )

    TextureLoaded loading (Just texture) ->
      -- 3) finally all done and we can create the layer record
      ( { model
        | queued = List.filter ((/=) loading) model.queued
        , animation = newLayer (Layer texture loading.name loading.base64) model.animation
        }
      , Cmd.none
      )

    TextureLoaded { name } Nothing ->
      ( { model | error = Just ("Failed to convert image: " ++ name) }
      , Cmd.none
      )

    SelectLayer idx ->
      ( { model | currentLayerIdx = clamp idx 0 (List.length model.animation.layers) }
      , Cmd.none
      )

    DeleteLayer idx ->
      { model | animation = deleteLayer idx model.animation }
      |> update (SelectLayer model.currentLayerIdx)

    NewFrame ->
      case newFrame model.currentFrameIdx model.animation of
        Just animation ->
          { model | animation = animation }
          |> update (SelectFrame (model.currentFrameIdx + 1))
        Nothing ->
          ( model, Cmd.none )

    SelectFrame idx ->
      ( { model | currentFrameIdx = clamp idx 0 (List.length model.animation.frames) }
      , Cmd.none
      )

    DeleteFrame idx ->
      { model | animation = deleteFrame model.currentFrameIdx model.animation }
      |> update (SelectFrame model.currentFrameIdx)

    TogglePlay ->
      ( { model
        | playing = not model.playing
        , animation = seek 0.0 model.animation
        }
      , Cmd.none
      )

    OnWindowResize _ _ ->
      ( model
      , Task.attempt CanvasRect (getElement "animation")
      )

    CanvasRect (Ok r) ->
      ( { model | canvas = Rect r.element.x r.element.y r.element.width r.element.height }
      , Cmd.none
      )

    CanvasRect (Err error) ->
      ( { model | error = Just (log "error getting size for canvas" (Debug.toString error)) }
      , Cmd.none
      )

    CanvasMouseDown e ->
      let
        clickedLayerIdx =
          if e.buttons == 1
          then findLayer (toCanvasCoords (mouseToPoint e) model.canvas) model.currentFrameIdx model.animation
          else Nothing
      in
        ( { model | currentLayerIdx = clickedLayerIdx |> withDefault model.currentLayerIdx }
        , Cmd.none
        )

    CanvasMouseMoved e ->
      let
          updateCurrentFrameLayer fn =
            { model | animation = updateFrameLayer model.currentFrameIdx model.currentLayerIdx fn model.animation }
      in
        ( case e.buttons of
            1 -> updateCurrentFrameLayer (moveLayerCoords e)
            2 -> updateCurrentFrameLayer (rotateLayer e model.canvas)
            _ -> model
        , Cmd.none )

    DragStart idx size which direction event ->
      let
        mouse = mouseToPoint event
      in
        ( { model | drag = Just (Drag idx which direction mouse mouse size) }
        , Cmd.none
        )

    DragMove event ->
      let
        updateMousePos drag =
          { drag | mouseAt = mouseToPoint event }
      in
        ( { model | drag = model.drag |> andThen (updateMousePos >> Just) }
        , Cmd.none
        )

    DragEnd event ->
      ( case model.drag of
          Just drag ->
            let
              fn = draggableUpdateFunc drag
              newIdx = drag.idx + (dragDistanceUnits drag)
            in
              { model
              | animation = fn drag.idx newIdx model.animation
              , drag = Nothing
              }
          Nothing ->
            model
      , Cmd.none
      )


draggableUpdateFunc : Drag -> (Int -> Int -> Animation -> Animation)
draggableUpdateFunc drag =
  case drag.which of
    DragLayer ->
      Animation.changeLayerOrder
    DragFrame ->
      Animation.changeFrameOrder


trans : Direction -> Int -> String
trans dir px =
  String.concat
    [ case dir of
        Horizontal -> "translateX("
        Vertical   -> "translateY("
    , String.fromInt px
    , "px)"
    ]


draggableStyle : Int -> Draggable -> (Maybe Drag) -> Attribute Msg
draggableStyle idx which maybeDrag =
  case maybeDrag of
    Just drag ->
      let
        diffUnits = dragDistanceUnits drag
        diffPixels = dragDistancePixels drag
      in
        if drag.idx == idx && drag.which == which then
          style "transform" ((trans drag.direction diffPixels) ++ " translateZ(10px)")
        else if idx < drag.idx && drag.idx + diffUnits <= idx then
          style "transform" (trans drag.direction drag.size)
        else if idx > drag.idx && drag.idx + diffUnits >= idx then
          style "transform" (trans drag.direction -drag.size)
        else
          style "" ""
    Nothing ->
      style "" ""


dragDistancePixels : Drag -> Int
dragDistancePixels drag =
  case drag.direction of
    Horizontal ->
      round (drag.mouseAt.x - drag.mouseStart.x)
    Vertical ->
      round (drag.mouseAt.y - drag.mouseStart.y)


dragDistanceUnits : Drag -> Int
dragDistanceUnits drag =
  (dragDistancePixels drag) // drag.size


toCanvasCoords : Point -> Rect -> Point
toCanvasCoords screen canvas =
  let center = Rect.center canvas
  in Point (screen.x - center.x) (screen.y - center.y)


mouseToPoint : MouseEvent -> Point
mouseToPoint event =
  Point event.client.x event.client.y


moveLayerCoords : MouseEvent -> FrameLayer -> FrameLayer
moveLayerCoords e fl =
  { fl | x = fl.x + e.movement.x, y = fl.y + e.movement.y }


rotateLayer : MouseEvent -> Rect -> FrameLayer -> FrameLayer
rotateLayer e canvas fl =
  let
    x = e.client.x - canvas.x - canvas.width / 2 - fl.x
    y = e.client.y - canvas.y - canvas.height / 2 - fl.y
    rotation = (atan2 (y + e.movement.y) (x + e.movement.x)) - (atan2 y x)
  in
    { fl | angle = fl.angle + rotation }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ onResize OnWindowResize
    , if model.playing
      then onAnimationFrameDelta AnimationFrame
      else Sub.none
    , if model.drag /= Nothing
      then Sub.batch
        [ onMouseMove (Decode.map DragMove mouseEventDecoder)
        , onMouseUp (Decode.map DragEnd mouseEventDecoder)
        ]
      else Sub.none
    ]
 


-- VIEW

lazy0 : (Html Msg) -> (Html Msg)
lazy0 x = lazy (\_ -> x) ()

view : Model -> Html Msg
view model =
  div [ class "main" ]
    [ lazy0 viewHeader
    , viewSidebar model
    , viewAnimation model
    , lazy viewPlayButton model.playing
    , viewFrames model
    ]


viewHeader : Html Msg
viewHeader =
  header []
    [ div [class "tabs"]
      [ a [class "button button-selected"] [text "File"]
      , a [class "button"] [text "Edit"]
      ]
    ]


viewSidebar : Model -> Html Msg
viewSidebar model =
  let
    button idx layer =
      div
        [ class "sidebar-layer"
        , draggableStyle idx DragLayer model.drag
        ]
        [ a
            [ classList [("button", True), ("button-selected", idx == model.currentLayerIdx)]
            , onClick (SelectLayer idx)
            , on "mousedown" (Decode.map (DragStart idx 30 DragLayer Vertical) mouseEventDecoder)
            , preventDefaultOn "selectstart" (Decode.succeed (NoOp, True))
            ]
            [ text layer.name ]
        , a [ class "sidebar-deleteLayer", onClick (DeleteLayer idx) ] []
        ]
  in
  section [class "sidebar"] (List.indexedMap button model.animation.layers)


viewAnimation : Model -> Html Msg
viewAnimation model =
  div
    [ id "animation"
    , class "animation"
    , on "mousedown" (Decode.map CanvasMouseDown mouseEventDecoder)
    , on "mousemove" (Decode.map CanvasMouseMoved mouseEventDecoder)
    , preventDefaultOn "contextmenu" (Decode.succeed (NoOp, True))
    ]
    [ viewCanvas model
    , div
      [ class "animation-empty"
      , hidden (model.animation.layers /= [])
      ]
      [text "Drop images here to get started"]
    , div
      [ classList
          [ ("animation-dropzone", True)
          , ("animation-dropzone-dropping", model.dropping)
          , ("animation-dropzone-empty", List.isEmpty model.animation.layers)
          ]
      , preventDefaultOn "dragenter" (Decode.succeed (OnDragEnter, True))
      , preventDefaultOn "dragover" (Decode.succeed (OnDragEnter, True))
      , preventDefaultOn "dragleave" (Decode.succeed (OnDragLeave, True))
      , custom "drop" (
          Decode.map (\x ->
            { message = x
            , stopPropagation = True
            , preventDefault = True
            }
          )
          dropDecoder
        )
      ]
      []
    , div
        [ class "animation-duration" ]
        [ text (String.fromFloat model.animation.elapsed) ]
    ]


viewCanvas : Model -> Html Msg
viewCanvas model =
  let
    clear =
      shapes [ fill Color.white ] [ rect (0, 0) model.canvas.width model.canvas.height ]
    center =
      translate (model.canvas.width / 2) (model.canvas.height / 2)
    load layer =
      Texture.loadFromImageUrl layer.base64 (TextureLoaded layer)
    framelayers =
      getAt model.currentFrameIdx model.animation.frames
        |> andThen (.layers >> Just)
        |> withDefault []
    draw fl =
      let { width, height } = Texture.dimensions fl.texture
      in Canvas.texture [transform [center, (translate fl.x fl.y), (rotate fl.angle)]] (-width / 2, -height / 2) fl.texture
  in
  Canvas.toHtmlWith
    { width = floor model.canvas.width
    , height = floor model.canvas.height
    , textures = List.map load model.queued
    }
    []
    (clear :: (List.map draw framelayers))


viewPlayButton : Bool -> Html Msg
viewPlayButton isPlaying =
  div
    [class "play"]
    [ div
        [ classList [("play-button", True), ("play-button-paused", not isPlaying)]
        , onClick TogglePlay
        ]
        []
    ]


viewFrames : Model -> Html Msg
viewFrames model =
  div [class "frames"]
    [ div [] (List.indexedMap viewFrame model.animation.frames)
    , div [ class "frames-add", onClick NewFrame ] []
    , div [ class "frames-remove", onClick (DeleteFrame model.currentFrameIdx) ] []
    ]


viewFrame : Int -> Frame -> Html Msg
viewFrame idx frame =
  div [class "frame", onClick (SelectFrame idx)] []



-- DECODERS


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


dropDecoder : Decode.Decoder Msg
dropDecoder =
  Decode.at ["dataTransfer","files"] (Decode.oneOrMore FilesDropped File.decoder)
