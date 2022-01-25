open System
open Common.ActivePatterns

type State = {
  Mem: Map<int64, int64>
  Mask: char array
}

let toBin (num: int64) =
  Convert.ToString(num, 2).PadLeft(36, '0').ToCharArray()

let getMemory state = state.Mem

let sumMemoryValues mem = Map.fold (fun sum _ value -> sum + value) 0L mem

let initialState = { Mem = Map.empty; Mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".ToCharArray() }

[<EntryPoint>]
let main _argv =
  let lines =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList
  
  let processInstructions modifyMemory =
    lines
      |> List.fold (fun state -> function
        | Regex @"mask = ([X10]*)" [mask] -> { state with Mask = mask.ToCharArray() }
        | Regex @"mem\[(\d+)\] = (\d+)" [Integer address; Integer value] ->
          { state with Mem = modifyMemory state address value }
        | _ -> state
      ) initialState
      |> getMemory
      |> sumMemoryValues

  let part1Result = processInstructions (fun state address value->
    let binValue = toBin value
    let maskedBinValue =
      Array.map2 (fun maskChar valueChar ->
        match maskChar with
        | 'X' -> valueChar
        | _ -> maskChar
      ) state.Mask binValue
      |> String

    let finalValue = Convert.ToInt64(maskedBinValue, 2)        

    state.Mem.Add(address, finalValue)  
  )

  let part2Result = processInstructions (fun state address value ->
    let binAddress = toBin address
    let maxIndex = 35
    
    let rec generateAddresses index addresses = 
      if index > maxIndex then
        addresses
      else
        let addressChar = binAddress[index]
        let maskChar = state.Mask[index]
        
        addresses
        |> List.collect (fun addressPart ->
          match maskChar with
          | '1' -> [List.append addressPart ['1']]
          | '0' -> [List.append addressPart [addressChar]]
          | 'X' ->
            ['1'; '0']
            |> List.map (fun floatingBit -> List.append addressPart [floatingBit])
          | _ -> [addressPart]
        )
        |> generateAddresses (index + 1)
  
    generateAddresses 0 [[]]
    |> List.map (fun addr -> Convert.ToInt64(addr |> List.toArray |> String, 2))
    |> List.fold (fun mem addr -> mem.Add(addr, value)) state.Mem
  )

  printfn "Part 1: %d" part1Result
  printfn "Part 2: %d" part2Result

  0