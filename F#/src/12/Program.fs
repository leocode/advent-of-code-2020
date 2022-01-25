open System
open Common.ActivePatterns

type Command =
  | MoveNorth of int
  | MoveSouth of int
  | MoveWest of int
  | MoveEast of int
  | TurnLeft of int
  | TurnRight of int
  | MoveForward of int

type Part1State = { X: int; Y: int; Rotation: int }

type Part2State =
  { ShipX: int
    ShipY: int;
    WaypointX: int;
    WaypointY: int }

let degreesToRad deg = Math.PI / 180.0 * float deg

// x2 = cos b * x1 - sin b y1
// y2 = sin b * x1 + cos b y1
let rotate x y deg =
  let rad = degreesToRad deg

  let x2 =
    cos (rad) * float x - sin (rad) * float y

  let y2 =
    sin (rad) * float x + cos (rad) * float y

  (int (round x2), int (round y2))

[<EntryPoint>]
let main _argv =
  let lines =
    System.IO.File.ReadLines "input.txt" |> Seq.toList

  let instructions =
    lines
    |> List.choose
         (function
         | Regex @"^(N|S|E|W|L|R|F)(\d+)$" [ command; Integer arg ] ->
           match command with
           | "N" -> Some(MoveNorth arg)
           | "S" -> Some(MoveSouth arg)
           | "E" -> Some(MoveEast arg)
           | "W" -> Some(MoveWest arg)
           | "L" -> Some(TurnLeft arg)
           | "R" -> Some(TurnRight arg)
           | "F" -> Some(MoveForward arg)
           | _ -> None
         | _ -> None)

  let part1 =
    instructions
    |> List.fold
         (fun state command ->
           match command with
           | MoveNorth distance -> { state with Y = state.Y + distance }
           | MoveSouth distance -> { state with Y = state.Y - distance }
           | MoveEast distance -> { state with X = state.X + distance }
           | MoveWest distance -> { state with X = state.X - distance }
           | TurnLeft degrees ->
             { state with
                 Rotation = (state.Rotation + 360 - degrees) % 360 }
           | TurnRight degrees ->
             { state with
                 Rotation = (state.Rotation + degrees) % 360 }
           | MoveForward distance ->
             match state.Rotation with
             | 0 -> { state with Y = state.Y + distance }
             | 90 -> { state with X = state.X + distance }
             | 180 -> { state with Y = state.Y - distance }
             | 270 -> { state with X = state.X - distance }
             | _ -> state)
         { X = 0; Y = 0; Rotation = 90 }

  let part2 =
    instructions
    |> List.fold
         (fun state command ->
           match command with
           | MoveNorth distance ->
             { state with
                 WaypointY = state.WaypointY + distance }
           | MoveSouth distance ->
             { state with
                 WaypointY = state.WaypointY - distance }
           | MoveEast distance ->
             { state with
                 WaypointX = state.WaypointX + distance }
           | MoveWest distance ->
             { state with
                 WaypointX = state.WaypointX - distance }
           | TurnLeft degrees ->
             let (x, y) =
               rotate state.WaypointX state.WaypointY degrees

             { state with
                 WaypointX = x;
                 WaypointY = y }
           | TurnRight degrees ->
             let (x, y) =
               rotate state.WaypointX state.WaypointY -degrees

             { state with
                 WaypointX = x;
                 WaypointY = y }
           | MoveForward distance ->
             { state with
                 ShipX = state.ShipX + state.WaypointX * distance;
                 ShipY = state.ShipY + state.WaypointY * distance })
         { ShipX = 0;
           ShipY = 0;
           WaypointX = 10;
           WaypointY = 1 }

  let part1Result = abs part1.X + abs part1.Y
  let part2Result = abs part2.ShipX + abs part2.ShipY

  0
