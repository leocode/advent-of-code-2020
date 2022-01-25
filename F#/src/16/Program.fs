open System
open Common.ActivePatterns

type TicketField = {
  Name: string
  ValidValueRanges: (int * int) list
}

type Ticket = int list

let satisfiesRange value field =
  field.ValidValueRanges
  |> List.exists (fun (min, max) -> value >= min && value <= max)

let isValidForAnyField fields value =
  fields
  |> List.exists (satisfiesRange value)

let findInvalidValues fields ticket =
  ticket
  |> List.filter (isValidForAnyField fields >> not)

let isTicketValid fields ticket =
  let invalidValues = findInvalidValues fields ticket
  
  invalidValues.Length = 0

let parseTicket (rawTicket: string) =
  rawTicket.Split ','
  |> Array.toList
  |> List.map (Int32.Parse)

let parseField = function
  | Regex @"([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)" [name; Integer min1; Integer max1; Integer min2; Integer max2;] ->
    Some { Name = name; ValidValueRanges = [(min1,  max1); (min2, max2)] }
  | _ -> None

let rec disambiguateFieldsPositions (fields: (TicketField * int list) list) =
  if fields.Length = 0 then
    []
  else
    let (field, [position]) :: remainingFields =
      fields
      |> List.sortBy (fun (_, validPositions) -> validPositions.Length)

    let remainingFields =
      remainingFields
      |> List.map (fun (field, positions) -> (field, List.except [position] positions))
    
    (field, position) :: disambiguateFieldsPositions remainingFields

[<EntryPoint>]
let main _argv =
  let lines =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList
  
  let [rawFields; _ :: [rawMyTicket]; _ :: rawNearbyTickets] =
    lines
    |> List.fold (fun state line ->
      if line = "" then
        [] :: state
      else
        let curr :: rest = state
        (List.append curr [line] ) :: rest
    ) [[]]
    |> List.rev
  
  let myTicket = parseTicket rawMyTicket
  let nearbyTickets = List.map parseTicket rawNearbyTickets
  let fields =
    rawFields
    |> List.choose parseField
  
  let part1Result =
    nearbyTickets
    |> List.collect (findInvalidValues fields)
    |> List.sum
  
  let validTickets =
    nearbyTickets
    |> List.filter (isTicketValid fields)
  
  let ticketValuesForPosition =
    validTickets
    |> List.transpose
    |> List.indexed
    
  let fieldsWithPossiblePositions =
    fields
    |> List.map (fun field ->
      let validPositions =
        ticketValuesForPosition
        |> List.choose (fun (position, values) ->
          let isValidPositionForField =
            values
            |> List.forall (fun value ->
              satisfiesRange value field
            )
          
          if isValidPositionForField then Some position else None
        )

      (field, validPositions)  
    )

  let fieldsWithExactPositions = disambiguateFieldsPositions fieldsWithPossiblePositions
  
  let part2Result =
    fieldsWithExactPositions
    |> List.filter (fun ({Name = name}, _) ->
      name.StartsWith "departure"
    )
    |> List.map (fun (_, position) ->
      int64 myTicket[position]
    )
    |> List.fold (*) 1L

  printfn $"Part 1: %d{part1Result}"
  printfn $"Part 2: %d{part2Result}"

  0