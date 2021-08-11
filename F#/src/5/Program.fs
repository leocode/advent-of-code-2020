// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print

let mapToComputableSequence oneChar chars =
  chars
  |> Array.mapi (fun i el -> (if el = oneChar then 1 else 0), i)

let compute sequence =
  sequence
  |> Array.fold (fun acc (el, i) ->
    acc - el * pown 2 (sequence.Length - i - 1)
  ) ((pown 2 sequence.Length) - 1)

let computeRow = mapToComputableSequence 'F' >> compute
let computeColumn = mapToComputableSequence 'L' >> compute

let computeSeatID row column = row * 8 + column
  
[<EntryPoint>]
let main argv =
  let lines =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList
  
  let seatIds =  
    lines
    |> List.map (fun line ->
      let chars = line.ToCharArray()
      
      let rowAddress = chars.[0..6]
      let columnAddress = chars.[7..]

      let row = computeRow rowAddress
      let column = computeColumn columnAddress
      let seatID = computeSeatID row column
      
      seatID
    )
  
  let part1Result = List.max seatIds
  let part2Result =
    [1..1022]
    |> List.find (fun potentialSeatId ->
      let containsLower = List.contains (potentialSeatId - 1)
      let containsUpper = List.contains (potentialSeatId + 1)
      let notContainsSeat = List.contains potentialSeatId >> not

      [containsLower; containsUpper; notContainsSeat]
      |> List.fold (fun acc fn -> acc && fn seatIds) true
    )
  
  printfn "Part 1: %d" part1Result
  printfn "Part 2: %d" part2Result
  
  0