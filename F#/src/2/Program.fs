open Common.ActivePatterns

type PasswordScheme = {
  Char: char
  Min: int
  Max: int
}

type PasswordEntry = {
  Scheme: PasswordScheme
  Password: string
}

type ValidationResult = Valid | Invalid

let ParseRawPassword line =
  match line with
  | Regex @"(\d+)-(\d+) (.): (.*)" [Integer min; Integer max; char; password;] ->
    Some {
      Scheme = {
        Min = min;
        Max = max;
        Char = char.[0];
      };
      Password = password;
    }
  | _ -> None

let SledRentalPolicyValidator entry =
  Option.map (fun { Scheme=scheme; Password=password } -> 
    let charCount =
      password
      |> String.filter (fun c -> c = scheme.Char)
      |> String.length
    if charCount >= scheme.Min && charCount <= scheme.Max then
      Valid
    else
      Invalid
  ) entry

let ContainsCharAtPos char pos (str: string) =
  let maybeCharAtPos = Array.tryItem pos (str.ToCharArray ())
  Option.contains char maybeCharAtPos

let OfficialTobogganCorporatePolicyValidator entry =
  Option.map (fun { Scheme=scheme; Password=password } ->
    // pos - 1, because they count from one and not from 0
    let onFirstPos = ContainsCharAtPos scheme.Char (scheme.Min - 1) password
    let onSecondPos = ContainsCharAtPos scheme.Char (scheme.Max - 1) password

    if (onFirstPos || onSecondPos) && not (onFirstPos && onSecondPos) then Valid else Invalid
  ) entry

let CountValidPasswords policyValidator passwords =
  passwords
    |> List.map (ParseRawPassword >> policyValidator)
    |> List.filter (Option.contains Valid)
    |> List.length

[<EntryPoint>]
let main argv =
  let rawPasswords =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList

  let part1Result = CountValidPasswords SledRentalPolicyValidator rawPasswords
  let part2Result = CountValidPasswords OfficialTobogganCorporatePolicyValidator rawPasswords

  printfn "Part 1: %d" part1Result
  printfn "Part 2: %d" part2Result

  0
