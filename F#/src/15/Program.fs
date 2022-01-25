open System

[<EntryPoint>]
let main _argv =
  let lines =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList

  let initialNumbers =
    lines[0].Split ','
    |> Array.toList
    |> List.map (fun n -> Convert.ToInt32(n, 10))
  
  let calculateNthNumber n =
    let initialMap =
      initialNumbers
      |> List.indexed
      |> List.fold (fun map (index, n) -> Map.add n (0, index + 1) map) Map.empty<int, int * int>

    Seq.unfold (fun (lastNumber, index, positionMap: Map<int, int * int>) ->
      let (lastLastPosition, lastPosition) = positionMap[lastNumber]
      
      let newNumber =
        if lastLastPosition = 0 then
          0
        else
          lastPosition - lastLastPosition
             
      let newPositionMap =
        if Map.containsKey newNumber positionMap then
          let (_, lastPosition) = positionMap[newNumber]
          Map.add newNumber (lastPosition, index) positionMap
        else
          Map.add newNumber (0, index) positionMap

      Some (newNumber, (newNumber, index + 1, newPositionMap))
    ) (List.last initialNumbers, List.length initialNumbers + 1, initialMap)
    |> Seq.take (n - (List.length initialNumbers))
    |> Seq.toList
    |> List.last
    
  let part1Result = calculateNthNumber 2020
  let part2Result = calculateNthNumber 30000000

  printfn $"Part 1: %d{part1Result}"
  printfn $"Part 2: %d{part2Result}"

  0