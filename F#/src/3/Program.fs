let add (a, b) (c, d) = (a + c, b + d)
let eq a b = a = b

type Tile = Tree | Ground | Unknown
type World = Tile[,]
type WorldDimensions = int * int
type Move = int * int

let MakeWorld (lines: List<string>) =
  lines
  |> List.map (fun line ->
    line.ToCharArray ()
    |> Array.map (fun char ->
      match char with
      | '#' -> Tree
      | '.' -> Ground
      | _ -> Unknown
    )
  )
  |> array2D
  
let GetWorldDimensions (world: World) =
  (world.[0, *].Length, world.[*, 0].Length)
  
let GetTile (world: World) position =
  world.[snd position, fst position]

let MakeRoute (worldX, worldY) move =
   let rec MakeMove currentPosition currentMove =
     match currentPosition with
     | (_, y) when y > (worldY - 1) -> []
     | _ ->
       let (nextPositionX, nextPositionY) = currentPosition |> add <| currentMove
       let normalizedNextPosition = (nextPositionX % worldX, nextPositionY)

       currentPosition :: MakeMove normalizedNextPosition currentMove
 
   MakeMove (0, 0) move

let CountTreesDownTheRoad world route =
  route
  |> List.map (GetTile world)
  |> List.filter (eq Tree)
  |> List.length

[<EntryPoint>]
let main _argv =
  let lines =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList
    
  let world = MakeWorld lines
  let worldDimensions = GetWorldDimensions world

  let part1Result =
    MakeRoute worldDimensions (3, 1)
    |> CountTreesDownTheRoad world

  let part2Result =
    [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)]
    |> List.map (MakeRoute worldDimensions >> CountTreesDownTheRoad world)
    |> List.fold ((*)) 1

  printfn "Part 1: %d" part1Result
  printfn "Part 2: %d" part2Result

  0
