open Common.ActivePatterns

type Instruction =
  | Nop of int
  | Acc of int
  | Jmp of int
  
type Program = Instruction list

type ExecutionState = {
  PC: int
  Accumulator: int
  AlreadyRunInstructions: Set<int>
  ExitCode: int
}

let runProgram (program: Program) =
  let initialState = {
    PC = 0
    Accumulator = 0
    AlreadyRunInstructions = Set.empty
    ExitCode = -1
  }
  
  let rec runInstruction state =
    if state.PC >= program.Length then
      { state with ExitCode = 0 }
    else
      let nextInstruction = program.[state.PC]
      
      if state.AlreadyRunInstructions.Contains state.PC then
        { state with ExitCode = 1 }
      else
        let newState = { state with AlreadyRunInstructions = state.AlreadyRunInstructions.Add state.PC }
        
        let nextState =
          match nextInstruction with
          | Nop _ -> { newState with PC = newState.PC + 1; }
          | Acc arg -> { newState with PC = newState.PC + 1; Accumulator = newState.Accumulator + arg }
          | Jmp arg -> { newState with PC = newState.PC + arg }
      
        runInstruction nextState

  runInstruction initialState

let ParseInput lines =
  lines
  |> List.choose (fun line ->
    match line with
    | Regex @"(nop|acc|jmp) ([+-]\d+)" [op; Integer arg] ->
      match op with
      | "nop" -> Some (Nop arg)
      | "acc" -> Some (Acc arg)
      | "jmp" -> Some (Jmp arg)
      | _ -> None
    | _ -> None
  )

let generatePotentiallyWorkingPrograms originalProgram =
  let originalProgramWithIndices = originalProgram |> List.indexed
  seq {
    for (index, instruction) in originalProgramWithIndices do
      match instruction with
      | Nop arg -> originalProgram.[0 .. index - 1] @ [Jmp arg] @ originalProgram.[index + 1..]
      | Jmp arg -> originalProgram.[0 .. index - 1] @ [Nop arg] @ originalProgram.[index + 1..]
      | _ -> ()
  }

[<EntryPoint>]
let main _argv =
  let lines =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList

  let program = ParseInput lines

  let part1Result = runProgram program
  let part2Result =
    program
    |> generatePotentiallyWorkingPrograms
    |> Seq.pick (fun potentiallyWorkingProgram ->
      let state = runProgram potentiallyWorkingProgram
      
      if state.ExitCode = 0 then Some state else None
    )

  printfn "Part 1: %d" part1Result.Accumulator
  printfn "Part 2: %d" part2Result.Accumulator

  0