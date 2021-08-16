open System

[<EntryPoint>]
let main _argv =
  let sortedAdapters =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map Int32.Parse
    |> List.sort

  let differences =
    sortedAdapters
    |> List.pairwise
    |> List.map (fun (n1, n2) -> n2 - n1)
    |> fun differences -> [List.head sortedAdapters] @ differences @ [3]

  let part1Result =
    differences
    |> List.groupBy id
    |> List.filter (fun (key, _) -> key = 1 || key = 3)
    |> List.fold (fun acc (_, list) -> acc * list.Length) 1

  let part2Result =
    differences
    |> List.fold (fun groupCounts difference ->
      let currentGroup::otherGroups = groupCounts
      
      if difference = 3 && currentGroup = 0 then
        groupCounts
      elif difference = 3 then
        [0] @ groupCounts
      else
        [currentGroup + 1] @ otherGroups
    ) [0]
    |> List.filter (fun n -> n > 1)
    |> List.map (fun n ->
      // number of possible combinations for the group of "ones"
      int64 (pown 2 (n - 1) - List.sum [1 .. n - 3])
    )
    |> List.fold (*) 1L

  printfn "Part 1: %d" part1Result
  printfn "Part 2: %d" part2Result

  0