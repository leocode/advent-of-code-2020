open System

let existsSumOfTwoDifferentNumbers numbers expectedSum =
  List.allPairs numbers numbers
  |> List.exists (fun (n1, n2) -> n1 <> n2 && n1 + n2 = expectedSum)

let findListOfContiguousNumbersSummingUpTo (inputNumbers: int64 list) expectedSum =
  let rec inner startIndex length =
    let potentialList = inputNumbers.[startIndex..startIndex + length]
    let sum = List.sum potentialList

    if sum = expectedSum then
      potentialList
    elif sum < expectedSum then
      inner startIndex (length + 1)
    else
      inner (startIndex + 1) 1
  
  inner 0 1

[<EntryPoint>]
let main _argv =
  let numbers =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map Int64.Parse

  let part1Result =
    numbers
    |> List.windowed 26
    |> List.pick (fun window ->
      let number :: numbers = List.rev window
      
      if not (existsSumOfTwoDifferentNumbers numbers number) then
        Some number
      else
        None
    )

  let part2Result =
    findListOfContiguousNumbersSummingUpTo numbers part1Result
    |> fun list -> List.min list + List.max list

  printfn "Part 1: %d" part1Result
  printfn "Part 2: %d" part2Result

  0