open System

let ParseInputIntoGroups (lines: string list) =
  lines
  |> List.map (fun line -> line.ToCharArray() |> Array.toList)
  |> List.fold (fun state chars ->
      if chars.Length = 0 then
        [[]] @ state
      else
        match state with
          | first::rest ->
            [[chars] @ first] @ rest
          | _ -> state
    ) [[]]

[<EntryPoint>]
let main argv =
  let lines =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList
  
  let groups = ParseInputIntoGroups lines
    
  let part1Result =
    groups
    |> List.sumBy (fun groupAnswers ->
      groupAnswers
      |> List.concat
      |> Set.ofList
      |> Set.count
    )
  
  let part2Result =
    groups
    |> List.sumBy (fun groupAnswers ->
      groupAnswers
      |> List.map Set.ofList
      |> List.reduce Set.intersect
      |> Set.count
    )
  
  printfn "Part 1: %d" part1Result
  printfn "Part 2: %d" part2Result
    
  0