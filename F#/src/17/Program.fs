let getActiveArea activeCells =
  activeCells
  |> List.transpose
  |> List.map (fun dimension -> ((List.min dimension) - 1, (List.max dimension) + 1))

let getNeighbours activeCells cell =
  activeCells
  |> List.filter (fun possibleNeighbour ->
    if possibleNeighbour = cell then
      false // filter out ourselves
    else
      let coordsDistances = List.map2 (fun coord1 coord2 -> abs (coord2 - coord1)) possibleNeighbour cell

      List.forall (fun distance -> distance = 0 || distance = 1) coordsDistances
  )

let times n fn arg =
  let rec inner curr prevResult =
    if curr = n then
      prevResult
    else
      inner (curr + 1) (fn prevResult)

  inner 0 arg

let crossProductBy fn list1 list2 =
  list1 |> List.collect (fun el1 -> list2 |> List.map (fn el1) )

let generateCellsInArea bounds =
  bounds
  |> List.map (fun (min, max) -> [ min .. max ] |> List.map (fun i -> [i]))
  |> List.reduce (crossProductBy (fun list1 list2 -> List.append list1 list2))

let step cells =
  let bounds = getActiveArea cells
  let cellsWithinBounds = generateCellsInArea bounds

  cellsWithinBounds
  |> List.choose (fun cell ->
    let neighbours = getNeighbours cells cell |> List.length

    let shouldStayActive = List.contains cell cells && (neighbours = 2 || neighbours = 3)
    let shouldActivate = not (List.contains cell cells) && neighbours = 3

    if shouldStayActive || shouldActivate then
      Some cell
    else
      None
  )

[<EntryPoint>]
let main _argv =
  let lines =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList

  let activeCells3d =
    lines
    |> List.indexed
    |> List.collect (fun (y, line) ->
      line.ToCharArray()
      |> Array.toList
      |> List.indexed
      |> List.choose (fun (x, char) ->
        if char = '#' then
          Some [x; y; 0]
        else
          None
      )
    )

  let activeCells4d =
    activeCells3d
    |> List.map (fun cells -> List.append cells [0])

  let part1Result =
    activeCells3d
    |> times 6 step
    |> List.length

  let part2Result =
    activeCells4d
    |> times 6 step
    |> List.length

  printfn $"Part 1: %d{part1Result}"
  printfn $"Part 2: %d{part2Result}"

  0
