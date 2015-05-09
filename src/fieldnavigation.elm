import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Time exposing (fps)
import Keyboard
import Window

import Model exposing (..)
import Vec2 exposing (..)



gridSize = 20
startPosition = (-gridSize, -gridSize)

andMap : List (a -> b) -> List a -> List b
andMap fnList aList = List.concatMap (\f -> List.map f aList) fnList

cartesianProduct : List a -> List b -> List (a, b)
cartesianProduct aList bList = andMap (List.map (,) aList) bList


------------------------------------------------------------------------------
-- MODEL

init : Model
init =
  { robotSpeed = 11
  , robot      = startPosition
  , goal       = ((gridSize, gridSize), (-3, 0), 1)
  , obstacles  = [Circle ((-12, 13), (7, 0), 4), Circle ((-12, -8), (4, 0), 6), Circle ((2, 3.5), (-8, 0), 5), Segment ((-7, 4), (1, -11), 1.5)]
  }


------------------------------------------------------------------------------
-- UPDATE

update : Float -> Model -> Model
update inputDT model =
  let dt = inputDT * 0.001 in
    { model
    | robot     <- updateRobot     dt model
    , goal      <- updateGoal      dt model
    , obstacles <- updateObstacles dt model
    }


updateRobot : Float -> Model -> Model.Position
updateRobot dt model =
  let (goal, _, radius) = model.goal
  in if
    | Vec2.length (Vec2.sub model.robot goal) < radius -> startPosition
    | otherwise -> Vec2.add model.robot <| Vec2.scale (dt * model.robotSpeed) (forceDirection model model.robot)


updateGoal : Float -> Model -> CircleData
updateGoal dt model =
  let
    (p, v, r) = model.goal
    (x, y)    = Vec2.add p (Vec2.scale dt v)
  in if
    | x < 0        -> ((-x, y), Vec2.scale (-1) v, r)
    | x > gridSize -> ((2*gridSize - x, y), Vec2.scale (-1) v, r)
    | otherwise    -> ((x, y), v, r)



updateObstacles : Float -> Model -> List Obstacle
updateObstacles dt model = List.map (updateObstacle dt) model.obstacles

updateObstacle : Float -> Model.Obstacle -> Model.Obstacle
updateObstacle dt obstacle = case obstacle of
  Circle (center, velocity, radius) ->
    let (x, y) = Vec2.add center <| Vec2.scale dt velocity in if
      | x < -gridSize -> Circle ((-2*gridSize - x, y), Vec2.scale (-1) velocity, radius)
      | x > gridSize  -> Circle ((2*gridSize - x, y), Vec2.scale (-1) velocity, radius)
      | otherwise     -> Circle ((x, y), velocity, radius)
  Segment s -> Segment s


forceDirection : Model -> Model.Position -> Model.Position
forceDirection model position =
  let
    (goal, _, _) = model.goal
    fG = Vec2.normalize (Vec2.sub goal position)
    fO = List.foldl Vec2.add (0, 0) <| List.map (obstacleForce position) model.obstacles
  in
    Vec2.normalize (Vec2.add fG fO)


obstacleForce : Model.Position -> Obstacle -> (Float, Float)
obstacleForce position obstacle = case obstacle of
  Circle (center, _, radius) ->
    let
      difference = Vec2.sub position center
      length     = Vec2.length difference
    in
      Vec2.scale (radius^2 / length^3) difference

  Segment (a, b, w) ->
    let
      ab = Vec2.sub b a
      ap = Vec2.sub position a
      bp = Vec2.sub position b
      apDist = Vec2.length ap
      bpDist = Vec2.length bp
      perp = Vec2.normalize (-(snd ab), fst ab)
      perpDist = Vec2.dot perp ap
    in if
      | Vec2.dot ab ap < 0 -> Vec2.scale (w^2 / apDist^3) ap
      | Vec2.dot ab bp > 0 -> Vec2.scale (w^2 / bpDist^3) bp
      | otherwise -> Vec2.scale (-(w^2) / perpDist^2) perp


------------------------------------------------------------------------------
-- VIEW

view : (Int, Int) -> Model -> Element
view (width, height) model =
  let
    (goalPosition, _, goalWidth) = model.goal
  in
    collage width height
      [ filled (rgb 64 128 255) <| rect (toFloat width) (toFloat height)
      , Graphics.Collage.scale 15 <| group
        [ group <| List.map drawObstacle model.obstacles
        , drawGrid model
        , move model.robot <| filled (rgb 200 200 40) <| circle 0.3
        , move goalPosition <| filled (rgb 40 200 40) <| circle 2
        ]
      ]


drawObstacle : Obstacle -> Form
drawObstacle obstacle =
  let obstacleColor = rgb 170 40 40
  in case obstacle of
    Circle (pos, _, size) ->
      move pos <| filled obstacleColor <| circle size

    Segment (pA, pB, width) ->
      let
        segmentStyle = { defaultLine | color <- obstacleColor, cap <- Round, width <- width }
      in
        traced segmentStyle <| segment pA pB


drawGrid : Model -> Form
drawGrid =
  let
    numbers = List.map (\x -> x + 0.5) [-gridSize..gridSize]
    centers = cartesianProduct numbers numbers
    segmentStyle = { defaultLine | color <- (rgb 200 200 200), width <- 0.05}
    t = traced segmentStyle
  in
    \model -> (group <| List.map (\p -> t <| segment p <| Vec2.add p <| Vec2.scale 0.8 <| forceDirection model p) centers)

------------------------------------------------------------------------------
-- SIGNALS

main : Signal Element
main = Signal.map2 view Window.dimensions (Signal.foldp update init (fps 60))
