import Browser
import Browser.Events exposing (onResize, onAnimationFrameDelta)
import Browser.Dom exposing (Element, Error, getElement)
import Html exposing (..)
import Html.Attributes exposing (id, class, classList, style, hidden, width, height)
import Html.Events exposing (on, onClick, preventDefaultOn, custom)
import Html.Lazy exposing (lazy)
import File exposing (File)
import File.Select as Select
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Dict
import Canvas
import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (transform, translate, rotate)
import Canvas.Texture as Texture exposing (Texture)
import Maybe exposing (andThen, withDefault)
import Debug exposing (log, toString)
import Color


-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type Easing
  = Linear
  | EaseIn
  | EaseOut

easingEnum = [Linear, EaseIn, EaseOut]

ease : Easing -> Float -> Float
ease which t =
  case which of
    Linear ->
      t
    EaseIn ->
      t^2
    EaseOut ->
      1 - (t - 1)^2


-- why our own mouse event type? because we want the `buttons`, `movementX` and `movementY` properties
-- of mouseevents that elm-pointer-events doesn't provide because safari doesn't support it
type alias MouseEvent =
  { buttons : Int
  , movementX : Float
  , movementY : Float
  , clientX : Float
  , clientY : Float
  , altKey : Bool
  , ctrlKey : Bool
  , metaKey : Bool
  }


type alias LayerLoading =
  { name : String
  , base64 : String
  }


type alias Layer =
  { id : Int
  , texture : Texture
  , name : String
  , base64 : String
  }


type alias FrameLayer =
  { layerId : Int
  , x : Float
  , y : Float
  , texture : Texture
  , angle : Float
  , easing : Easing
  }

newFrameLayer id texture =
  FrameLayer id 0.0 0.0 texture 0.0 Linear


type alias Frame =
  { id : Int
  , duration : Float
  , easing : Easing
  , layers : List FrameLayer
  }


type alias Rect =
  { x : Float
  , y : Float
  , width : Float
  , height : Float
   }


type alias Model =
  { dropping : Bool
  , playing : Bool
  , identity : Int
  , elapsed : Float
  , canvas : Rect
  , queued : List LayerLoading
  , frames : List Frame
  , selectedFrameId : Int
  , layers : List Layer
  , selectedLayerId : Int
  , error : String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model
      False -- dropping
      False -- playing
      1 -- identity
      0.0 -- elapsed
      (Rect 0.0 0.0 0.0 0.0) -- canvas
      [] -- queued
      [Frame 0 0.3 Linear []] -- frames
      0 -- selectedFrameId
      [] -- layers
      -1 -- selectedLayerId
      "" -- error
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
  | SelectLayer Layer
  | DeleteLayer Layer
  | SelectFrame Frame
  | TogglePlay
  | OnWindowResize Int Int
  | CanvasRect (Result Error Element)
  | MouseMoved MouseEvent


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )

    AnimationFrame delta ->
      ( { model | elapsed = model.elapsed + delta }
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
      let
        newLayer =
          (Layer model.identity texture loading.name loading.base64)
        addFrameLayer frame =
          { frame | layers = (newFrameLayer model.identity texture) :: frame.layers }
      in
      ( { model
        | identity = model.identity + 1
        , queued = List.filter ((/=) loading) model.queued
        , layers = newLayer :: model.layers
        , selectedLayerId = model.identity
        , frames = List.map addFrameLayer model.frames
        }
      , Cmd.none
      )

    TextureLoaded { name } Nothing ->
      ( { model | error = "Failed to convert image: " ++ name }
      , Cmd.none
      )

    SelectLayer layer ->
      ( model, Cmd.none )

    DeleteLayer layer ->
      let
        deleteFromFrame frame =
          { frame | layers = List.filter (\l -> l.layerId /= layer.id) frame.layers }
      in
        ( { model
          | layers = List.filter ((/=) layer) model.layers
          , frames = List.map deleteFromFrame model.frames
          }
        , Cmd.none
        )

    SelectFrame frame ->
      ( model
      , Cmd.none
      )

    TogglePlay ->
      ( { model | playing = not model.playing, elapsed = 0.0 }
      , Cmd.none
      )

    OnWindowResize _ _ ->
      -- TODO: can you chain commands without going through the update function? this seems silly
      ( model
      , Task.attempt CanvasRect (getElement "animation")
      )

    CanvasRect (Ok r) ->
      ( { model | canvas = Rect r.element.x r.element.y r.element.width r.element.height }
      , Cmd.none
      )

    CanvasRect (Err error) ->
      ( { model | error = log "error getting size for canvas" (Debug.toString error) }
      , Cmd.none
      )

    MouseMoved e ->
      ( case e.buttons of
          1 -> updateFrameLayer model (moveLayer e)
          2 -> updateFrameLayer model (rotateLayer e model.canvas)
          _ -> model
      , Cmd.none )


moveLayer : MouseEvent -> FrameLayer -> FrameLayer
moveLayer e fl =
  { fl | x = fl.x + e.movementX, y = fl.y + e.movementY }


rotateLayer : MouseEvent -> Rect -> FrameLayer -> FrameLayer
rotateLayer e canvas fl =
  let
    x = e.clientX - canvas.x - canvas.width / 2 - fl.x
    y = e.clientY - canvas.y - canvas.height / 2 - fl.y
    rotation = (atan2 (y + e.movementY) (x + e.movementX)) - (atan2 y x)
  in
    { fl | angle = fl.angle + rotation }


zip : (List a) -> (List b) -> (List (a, b))
zip = List.map2 Tuple.pair


findBy : (a -> b) -> (b -> Bool) -> (List a) -> Maybe a
findBy transform predicate list =
  List.filter (transform >> predicate) list |> List.head


updateIf : (a -> Bool) -> (a -> a) -> List a -> List a
updateIf predicate transform list =
  let
    maybeUpdate x =
      if predicate x
      then transform x
      else x
  in
    List.map maybeUpdate list


updateFrame : Model -> (Frame -> Frame) -> Model
updateFrame model transform =
  { model | frames = updateIf (\f -> f.id == model.selectedFrameId) transform model.frames }


updateLayer : Model -> (Layer -> Layer) -> Model
updateLayer model transform =
  { model | layers = updateIf (\l -> l.id == model.selectedLayerId) transform model.layers }


updateFrameLayer : Model -> (FrameLayer -> FrameLayer) -> Model
updateFrameLayer model transform =
  let
    updateFrameLayers frame =
      { frame | layers = updateIf (\l -> l.layerId == model.selectedLayerId) transform frame.layers }
  in
    { model | frames = updateIf (\f -> f.id == model.selectedFrameId) updateFrameLayers model.frames }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ onResize OnWindowResize
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
    button layer =
      div
        [class "sidebar-layer"]
        [ a [class "button"] [text layer.name]
        , a [class "sidebar-deleteLayer", onClick (DeleteLayer layer)] []
        ]
  in
  section [class "sidebar"] (List.map button model.layers)


viewAnimation : Model -> Html Msg
viewAnimation model =
  div
    [ id "animation"
    , class "animation"
    , on "mousemove" (Decode.map MouseMoved mouseEventDecoder)
    , preventDefaultOn "contextmenu" (Decode.succeed (NoOp, True))
    ]
    [ viewCanvas model
    , div
      [ class "animation-empty"
      , hidden (model.layers /= [])
      ]
      [text "Drop images here to get started"]
    , div
      [ classList
          [ ("animation-dropzone", True)
          , ("animation-dropzone-dropping", model.dropping)
          , ("animation-dropzone-empty", List.isEmpty model.layers)
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
        [ text (String.fromFloat model.elapsed) ]
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
      findBy .id ((==) model.selectedFrameId) model.frames
        |> andThen (.layers >> Just)
        |> withDefault [] 
    draw fl =
      let { width, height } = Texture.dimensions fl.texture
      in Canvas.texture [transform [center, (translate fl.x fl.y), (rotate fl.angle)]] (-width/2, -height/2) fl.texture
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
  div [class "frames"] (List.map viewFrame model.frames)


viewFrame : Frame -> Html Msg
viewFrame frame =
  div [class "frame", onClick (SelectFrame frame)] []



-- DECODERS


mouseEventDecoder : Decode.Decoder MouseEvent
mouseEventDecoder =
  Decode.map8 MouseEvent
    (Decode.field "buttons" Decode.int)
    (Decode.field "movementX" Decode.float)
    (Decode.field "movementY" Decode.float)
    (Decode.field "clientX" Decode.float)
    (Decode.field "clientY" Decode.float)
    (Decode.field "altKey" Decode.bool)
    (Decode.field "ctrlKey" Decode.bool)
    (Decode.field "metaKey" Decode.bool)


dropDecoder : Decode.Decoder Msg
dropDecoder =
  Decode.at ["dataTransfer","files"] (Decode.oneOrMore FilesDropped File.decoder)
