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
import Events exposing (MouseEvent, mouseEventDecoder)
import Orderable exposing (Direction(..), draggableAttributes, containerStyle)
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


type alias LayerLoading =
  { name : String
  , base64 : String
  }


type OrderableType
  = OrderLayer
  | OrderFrame


type alias Model =
  { dropping : Bool
  , playing : Bool
  , canvas : Rect
  , queued : List LayerLoading
  , animation : Animation
  , currentFrameIdx : Int
  , currentLayerIdx : Int
  , reOrder : Maybe (Orderable.State OrderableType)
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
      Nothing -- reOrder
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
  | OrderableMsg (Orderable.Msg OrderableType)


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
          then findLayer (toCanvasCoords (e.client) model.canvas) model.currentFrameIdx model.animation
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

    OrderableMsg (Orderable.Apply state newIdx) ->
      ( { model | animation = (draggableUpdateFunc state.which) state.idx newIdx model.animation }
      , Cmd.none
      )

    OrderableMsg reMsg ->
      let
        (reOrder, cmd) = Orderable.update OrderableMsg reMsg model.reOrder
      in
        ( { model | reOrder = reOrder }, cmd )


draggableUpdateFunc : OrderableType -> (Int -> Int -> Animation -> Animation)
draggableUpdateFunc orderable =
  case orderable of
    OrderLayer ->
      Animation.changeLayerOrder
    OrderFrame ->
      Animation.changeFrameOrder


toCanvasCoords : Point -> Rect -> Point
toCanvasCoords screen canvas =
  let center = Rect.center canvas
  in Point (screen.x - center.x) (screen.y - center.y)


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
    , Orderable.subscriptions model.reOrder OrderableMsg
    , if model.playing
      then onAnimationFrameDelta AnimationFrame
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
        ( (class "sidebar-layer")
        :: draggableAttributes idx 30 OrderLayer Vertical OrderableMsg model.reOrder
        )
        [ a
            [ classList [("button", True), ("button-selected", idx == model.currentLayerIdx)]
            , onClick (SelectLayer idx)
            ]
            [ text layer.name ]
        , a [ class "sidebar-deleteLayer", onClick (DeleteLayer idx) ] []
        ]
  in
  section
    [ class "sidebar", containerStyle ]
    (List.indexedMap button model.animation.layers)


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
    [ div [ containerStyle ] (List.indexedMap (viewFrame model.reOrder) model.animation.frames)
    , div [ class "frames-add", onClick NewFrame ] []
    , div [ class "frames-remove", onClick (DeleteFrame model.currentFrameIdx) ] []
    ]


viewFrame : Maybe (Orderable.State OrderableType) -> Int -> Frame -> Html Msg
viewFrame reOrder idx frame =
  div
    ( [class "frame", onClick (SelectFrame idx) ]
    ++ draggableAttributes idx 90 OrderFrame Horizontal OrderableMsg reOrder
    )
    []



-- DECODERS


dropDecoder : Decode.Decoder Msg
dropDecoder =
  Decode.at ["dataTransfer","files"] (Decode.oneOrMore FilesDropped File.decoder)
