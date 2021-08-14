open System
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
  else None

type InclusionRule = {
  Count: int
  SourceBagName: string
  TargetBagName: string
}

let ruleRegex = Regex @"^([a-z ]+?) bags contain (?:no other bags|(\d+[a-z ]+?) (?:bags|bag)(?:, )?)+.$"

let ParseRule input =
  let m = ruleRegex.Match input
  
  if not m.Success then [] else    
    let actualGroups = m.Groups |> List.ofSeq |> List.tail
    
    // not fully type safe, but for this input we are sure that it will work
    let [bagNameGroup; inclusionRulesGroup] = actualGroups
    
    inclusionRulesGroup.Captures
    |> List.ofSeq
    |> List.choose (fun c ->
      match c.Value with
      | Regex @"(\d+) ([a-z ]+)" [count; bagName] ->
        Some {
          Count = Int32.Parse(count)
          SourceBagName = bagNameGroup.Value
          TargetBagName = bagName
        }
      | _ -> None
    )

let ParseRules lines =
  lines
  |> List.collect ParseRule

let countAllBagsContainingEventuallyThis rules bagName =
  let rec inner targetBagName =
    rules
    |> List.filter (fun rule ->
      rule.TargetBagName = targetBagName  
    )
    |> List.collect (fun rule -> [rule.SourceBagName] @ inner rule.SourceBagName)
  
  inner bagName |> Set.ofList |> Set.count

let countAllBagsContainedInside rules bagName =
  let rec inner sourceBagName =
    rules
    |> List.filter (fun rule ->
      rule.SourceBagName = sourceBagName
    )
    |> List.fold (fun acc rule ->
      acc + rule.Count + rule.Count * inner rule.TargetBagName  
    ) 0
  
  inner bagName

[<EntryPoint>]
let main argv =
  let lines =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList
  
  let rules = ParseRules lines
  
  let part1Result = countAllBagsContainingEventuallyThis rules "shiny gold"
  let part2Result = countAllBagsContainedInside rules "shiny gold"
  
  printfn "Part 1: %d" part1Result
  printfn "Part 2: %d" part2Result
  
  0