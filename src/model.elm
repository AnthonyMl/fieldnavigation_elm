module Model where


type alias CircleData = {
  position : (Float, Float),
  velocity : (Float, Float),
  radius   : Float
}

type alias SegmentData = {
  a     : (Float, Float),
  b     : (Float, Float),
  width : Float
}

type Obstacle = Circle CircleData | Segment SegmentData

type alias Model = {
  robotPosition : (Float, Float),
  goal          : CircleData,
  obstacles     : List Obstacle
  }
