module Model where


type alias Position = (Float, Float)
type alias Velocity = (Float, Float)

type alias CircleData = (Position, Velocity, Float)

type alias SegmentData = (Position, Position, Float)

type Obstacle = Circle CircleData | Segment SegmentData

type alias Model = {
  robotSpeed : Float,
  robot      : Position,
  goal       : CircleData,
  obstacles  : List Obstacle
  }
