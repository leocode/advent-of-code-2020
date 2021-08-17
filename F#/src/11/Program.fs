open System

type Tile =
  | Floor
  | EmptySeat
  | OccupiedSeat

type World = Tile[,]

let initializeWorld (lines: string list) =
  let world = Array2D.create lines.Head.Length lines.Length Floor

  lines
    |> List.iteri (fun y line ->
      line.ToCharArray() |> Array.iteri (fun x char ->
        if char = 'L' then world.[x,y] <- EmptySeat
      )  
    )

  world

let worldToList (world: World) =
  world
  |> Seq.cast<Tile> // convert 2d array to 1d sequence
  |> Seq.toList

let getNeighbours (world: World) x y =
  let element = world.[x, y]
  
  let slice =
    world.[x-1 .. x+1, y-1 .. y+1]
    |> worldToList
  
  let firstElementIndex = List.findIndex ((=) element) slice
  
  slice.[0 .. firstElementIndex - 1] @ slice.[firstElementIndex + 1 ..]

let validateCoords world (x, y) =
  x >= 0 && y >= 0 && x < Array2D.length1 world && y < Array2D.length2 world
 
let applyVector (vx, vy) (px, py) =
  (vx + px, vy + py)
  
let findFirstSeatInDirection world start direction =
  let rec loop point =
    let nextPoint = applyVector direction point
    let (nx, ny) = nextPoint
    
    if validateCoords world nextPoint then
      let tile = world.[nx, ny]
      
      match tile with
      | EmptySeat | OccupiedSeat -> Some tile
      | _ -> loop nextPoint
    else
      None
    
  loop start

let getVisibleSeats (world: World) observationPoint =
  let directions = [(-1, 0); (-1, -1); (0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1)]

  directions
  |> List.choose (findFirstSeatInDirection world observationPoint)

let countOccupiedSeats tiles =
  tiles
  |> List.filter (function | OccupiedSeat -> true | _ -> false)
  |> List.length

let stepWorld world =
  world
  |> Array2D.mapi (fun x y tile ->
    let neighbours  = getNeighbours world x y
    let occupiedNeighbours = countOccupiedSeats neighbours
    
    match tile with
    | EmptySeat when occupiedNeighbours = 0 -> OccupiedSeat
    | OccupiedSeat when occupiedNeighbours >= 4 -> EmptySeat
    | _ -> tile
  )

let stepWorld2 world =
  world
  |> Array2D.mapi (fun x y tile ->
    let visibleSeats  = getVisibleSeats world (x, y)
    let occupiedVisibleSeats = countOccupiedSeats visibleSeats
    
    match tile with
    | EmptySeat when occupiedVisibleSeats = 0 -> OccupiedSeat
    | OccupiedSeat when occupiedVisibleSeats >= 5 -> EmptySeat
    | _ -> tile
  )

let runWorldUntilSettlement world stepWorld =
  let rec loop previousWorld =
    let newWorld = stepWorld previousWorld
    
    if newWorld = previousWorld then
      newWorld
    else
      loop newWorld
    
  loop world

[<EntryPoint>]
let main argv =
  let lines =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList

  let world = initializeWorld lines
  let finalWorld = runWorldUntilSettlement world stepWorld
  
  let part1Result =
    finalWorld
    |> worldToList
    |> countOccupiedSeats
    
  let finalWorld2 = runWorldUntilSettlement world stepWorld2

  let part2Result =
    finalWorld2
    |> worldToList
    |> countOccupiedSeats

  printfn "Part 1: %d" part1Result
  printfn "Part 2: %d" part2Result

  0