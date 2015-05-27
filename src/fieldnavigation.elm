import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (rgb)
import Time exposing (fps)
import Window

import Model exposing (..)
import Vec2 exposing (..)



gridSize = 20
robotSpeed = 11
startPosition = (-gridSize, -gridSize)



andMap : List (a -> b) -> List a -> List b
andMap fnList aList = List.concatMap (\f -> List.map f aList) fnList


cartesianProduct : List a -> List b -> List (a, b)
cartesianProduct aList bList = andMap (List.map (,) aList) bList


------------------------------------------------------------------------------
-- MODEL

init : Model
init =
  { robotPosition = startPosition
  , goal          = { position = (gridSize, gridSize), velocity = (-3, 0), radius = 1 }
  , obstacles     =
      [ Circle
        { position = (-12, 13)
        , velocity = (7, 0)
        , radius   = 4
        },
        Circle
        { position = (-12, -8)
        , velocity = (4, 0)
        , radius   = 6
        },
        Circle
        { position = (2, 3.5)
        , velocity = (-8, 0)
        , radius   = 5
        },
        Segment
        { a        = (-7, 4)
        , b        = (1, -11)
        , width    = 1.5
        }
      ]
  }


------------------------------------------------------------------------------
-- UPDATE

update : Float -> Model -> Model
update inputDT model =
  let dt = inputDT * 0.001 in
    { model
    | robotPosition <- updateRobot     dt model
    , goal          <- updateGoal      dt model
    , obstacles     <- updateObstacles dt model
    }


updateRobot : Float -> Model -> (Float, Float)
updateRobot dt model = if
  | Vec2.length (Vec2.sub model.robotPosition model.goal.position) < model.goal.radius -> startPosition
  | otherwise -> Vec2.add model.robotPosition <| Vec2.scale (dt * robotSpeed) (forceDirection model model.robotPosition)


updateGoal : Float -> Model -> CircleData
updateGoal dt model =
  let
    (x, y) = Vec2.add model.goal.position <| Vec2.scale dt model.goal.velocity
    (newPosition, newVelocity) = if
      | x < 0        -> ((            -x, y), Vec2.scale (-1) model.goal.velocity)
      | x > gridSize -> ((2*gridSize - x, y), Vec2.scale (-1) model.goal.velocity)
      | otherwise    -> ((             x, y),                 model.goal.velocity)
  in
    { position = newPosition
    , velocity = newVelocity
    , radius   = model.goal.radius
    }


updateObstacles : Float -> Model -> List Obstacle
updateObstacles dt model = List.map (updateObstacle dt) model.obstacles


updateObstacle : Float -> Model.Obstacle -> Model.Obstacle
updateObstacle dt obstacle = case obstacle of

  Circle c ->
    let
      (x, y) = Vec2.add c.position <| Vec2.scale dt c.velocity
      (newPosition, newVelocity) = if
        | x < -gridSize -> ((-2*gridSize - x, y), Vec2.scale (-1) c.velocity)
        | x >  gridSize -> (( 2*gridSize - x, y), Vec2.scale (-1) c.velocity)
        | otherwise     -> ((              x, y),                 c.velocity)
    in Circle
      { position = newPosition
      , velocity = newVelocity
      , radius   = c.radius
      }

  Segment s -> Segment s


forceDirection : Model -> (Float, Float) -> (Float, Float)
forceDirection model position =
  let
    fG = Vec2.normalize <| Vec2.sub model.goal.position position
    fO = List.foldl Vec2.add (0, 0) <| List.map (obstacleForce position) model.obstacles
  in
    Vec2.normalize (Vec2.add fG fO)


obstacleForce : (Float, Float) -> Obstacle -> (Float, Float)
obstacleForce position obstacle = case obstacle of

  Circle c ->
    let
      difference = Vec2.sub position c.position
    in
      Vec2.scale (c.radius^2 / (Vec2.length difference)^3) difference

  Segment s ->
    let
      ab = Vec2.sub      s.b s.a
      ap = Vec2.sub position s.a
      bp = Vec2.sub position s.b
      apDist = Vec2.length ap
      bpDist = Vec2.length bp
      perp = Vec2.normalize (-(snd ab), fst ab)
      perpDist = Vec2.dot perp ap
    in if
      | Vec2.dot ab ap < 0 -> Vec2.scale (  s.width^2  / apDist^3  ) ap
      | Vec2.dot ab bp > 0 -> Vec2.scale (  s.width^2  / bpDist^3  ) bp
      | otherwise          -> Vec2.scale (-(s.width^2) / perpDist^2) perp


------------------------------------------------------------------------------
-- VIEW

view : (Int, Int) -> Model -> Element
view (width, height) model =
  collage width height
    [ filled (rgb 64 128 255) <| rect (toFloat width) (toFloat height)
    , Graphics.Collage.scale 15 <| group
      [ group <| List.map drawObstacle model.obstacles
      , drawGrid model
      , move model.robotPosition <| filled (rgb 200 200 40) <| circle 0.3
      , move model.goal.position <| filled (rgb 40 200 40)  <| circle 2
      ]
    ]


drawObstacle : Obstacle -> Form
drawObstacle obstacle =
  let obstacleColor = rgb 170 40 40
  in case obstacle of

    Circle c -> move c.position <| filled obstacleColor <| circle c.radius

    Segment s -> traced { defaultLine | color <- obstacleColor, cap <- Round, width <- s.width } <| segment s.a s.b


drawGrid : Model -> Form
drawGrid =
  let
    numbers = List.map (\x -> x + 0.5) [-gridSize..gridSize]
    centers = cartesianProduct numbers numbers
    tracedStyle = traced { defaultLine | color <- rgb 200 200 200, width <- 0.05 }
  in
    \model -> group <| List.map (\p -> tracedStyle <| segment p <| Vec2.add p <| Vec2.scale 0.8 <| forceDirection model p) centers


------------------------------------------------------------------------------
-- SIGNALS

main : Signal Element
main = Signal.map2 view Window.dimensions (Signal.foldp update init (fps 60))
