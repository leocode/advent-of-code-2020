open System

let findTimestamp buses =
  let rec inner possibleTimestamp =
    let rems =
      buses
      |> List.map (fun (diffFromTimeToDeparture, id) -> (((int64 id) - (possibleTimestamp % id)) % id) - diffFromTimeToDeparture)
      |> List.forall ( fun i -> i = 0L)

    if rems then
      possibleTimestamp
    else
      inner (possibleTimestamp + 1L)

  inner 100000000000000L

[<EntryPoint>]
let main _argv =
  let lines =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList

  let departureTimeEstimate = Int32.Parse(lines[0])
  let busesForPart1 =
    lines[1].Split ','
    |> Array.toList
    |> List.filter (fun id -> id <> "x")
    |> List.map Int32.Parse

  let part1Result =
    busesForPart1
    |> List.map (fun id -> (id, (id - (departureTimeEstimate % id)) % id)) // we are doing second mod to account for 0 reminder case
    |> List.sortBy snd
    |> List.head
    |> fun (id, minutestToDeparture) -> id * minutestToDeparture

  let busesForPart2 =
    lines[1].Split ','
    |> Array.toList
    |> List.indexed
    |> List.filter (fun (_, id) -> id <> "x")
    |> List.map (fun (index, id) -> (int64 index, Int64.Parse id))

  let part2Result = findTimestamp busesForPart2

  printfn $"Part 1: %d{part1Result}"
  printfn $"Part 2: %d{part2Result}"

  0
