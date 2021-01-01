[<EntryPoint>]
let main argv =
  let lines = System.IO.File.ReadLines "input.txt"

  let rec makePairs list =
    match list with
      | [] -> []
      | first::rest ->
        rest
        |> List.map (fun i -> (first, i))
        |> List.append (makePairs rest)

  let result =
    lines
    |> List.ofSeq
    |> List.map int
    |> makePairs
    |> List.filter (fun (a, b) -> a + b = 2020) 
    |> List.map (fun (a, b) -> a * b)
    |> List.head
  
  System.Console.WriteLine result

  0
