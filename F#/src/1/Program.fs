let rec makeCombinationsWithLength length list =
  match length with
    | 1 ->
      list
      |> List.map (fun i -> [i])
    | _ ->
      match list with
        | [] -> []
        | first::rest ->
          rest
          |> makeCombinationsWithLength (length - 1)
          |> List.map (fun i -> List.append [first] i)
          |> List.append (makeCombinationsWithLength length rest)

let makeTripples list = makeCombinationsWithLength 3 list
let makePairs list = makeCombinationsWithLength 2 list
let findSolution combinations =
  combinations
  |> List.filter (fun i -> List.sum i = 2020) 
  |> List.map (fun i -> List.fold (( * )) 1 i)
  |> List.head

let Part1 input =
  input
  |> makePairs
  |> findSolution

let Part2 input =
  input
  |> makeTripples
  |> findSolution

[<EntryPoint>]
let main argv =
  let lines =
    System.IO.File.ReadLines "input.txt"
    |> List.ofSeq
    |> List.map int
  
  printfn "Part 1: %d" (Part1 lines)
  printfn "Part 2: %d" (Part2 lines)

  0
