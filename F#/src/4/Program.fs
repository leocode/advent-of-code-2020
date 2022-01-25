open System
open System.Text.RegularExpressions

open FSharpx
open FSharpx.Option

open Common.ActivePatterns

let isValidEyeColor str =
  ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] |> List.contains str

let isValidPid str =
  Regex.IsMatch(str, @"^\d{9}$")
  
let isValidHairColor str =
  Regex.IsMatch(str, @"^#[0-9a-f]{6}$")
  
let isValidYearInRange min max str =
  if Regex.IsMatch(str, @"^\d{4}$") then
    let maybeYear = Int32.parse str
    
    match maybeYear with
    | Some year when year >= min && year <= max -> true
    | _ -> false
  else
    false
    
let isValidBirthYear = isValidYearInRange 1920 2002
let isValidIssueYear = isValidYearInRange 2010 2020

let isValidExpirationYear = isValidYearInRange 2020 2030

let isValidHeight str =
  match str with
  | Regex @"^(\d+)(cm|in)$" [number; unit] ->
    let parsedNumber = Int32.Parse number // we validated it earlier
    
    match unit with
    | "in" when parsedNumber >=59 && parsedNumber <= 76 -> true
    | "cm" when parsedNumber >= 150 && parsedNumber <= 193 -> true
    | _ -> false
  | _ -> false

// byr (Birth Year)
// iyr (Issue Year)
// eyr (Expiration Year)
// hgt (Height)
// hcl (Hair Color)
// ecl (Eye Color)
// pid (Passport ID)
// cid (Country ID)

type Byr = Byr of string
type Iyr = Iyr of string
type Eyr = Eyr of string
type Hgt = Hgt of string
type Hcl = Hcl of string
type Ecl = Ecl of string
type Pid = Pid of string
type Cid = Cid of string

type Passport = {
  Byr: Byr
  Iyr: Iyr
  Eyr: Eyr
  Hgt: Hgt
  Hcl: Hcl
  Ecl: Ecl
  Pid: Pid
  Cid: Cid option
}

let findField name (fields: string list) =
  fields
  |> List.choose (fun field ->
      match field.Split [| ':' |] |> Array.toList with
      | [label; value] when label = name -> Some value
      | _ -> None
    )
  |> List.tryHead

let createField<'a> ctor (f: string option): 'a option = f |> Option.map ctor

let createByr fields = createField Byr (findField "byr" fields)
let createIyr fields = createField Iyr (findField "iyr" fields)
let createEyr fields = createField Eyr (findField "eyr" fields)
let createHgt fields = createField Hgt (findField "hgt" fields)
let createHcl fields = createField Hcl (findField "hcl" fields)
let createEcl fields = createField Ecl (findField "ecl" fields)
let createPid fields = createField Pid (findField "pid" fields)
let createCid fields = createField Cid (findField "cid" fields)

let ParseInput (lines: string list) =
  lines
  |> List.collect (fun line -> line.Split [| ' ' |] |> Array.toList)
  |> List.fold (fun state a ->
      if a = "" then
        [[]] @ state
      else
        match state with
          | first::rest ->
            [[a] @ first] @ rest
          | _ -> state
    ) [[]]
  |> List.map (fun fields ->
      maybe {
        let! byr = createByr fields
        let! iyr = createIyr fields
        let! eyr = createEyr fields
        let! hgt = createHgt fields
        let! hcl = createHcl fields
        let! ecl = createEcl fields
        let! pid = createPid fields
        let cid = createCid fields
        
        return {
          Byr = byr
          Iyr = iyr
          Eyr = eyr
          Hgt = hgt
          Hcl = hcl
          Ecl = ecl
          Pid = pid
          Cid = cid
        }
      }
  )
  |> List.choose id

let isPassportValid passport =
  let (Byr byr) = passport.Byr
  let (Iyr iyr) = passport.Iyr
  let (Eyr eyr) = passport.Eyr
  let (Hgt hgt) = passport.Hgt
  let (Hcl hcl) = passport.Hcl
  let (Ecl ecl) = passport.Ecl
  let (Pid pid) = passport.Pid

  isValidBirthYear byr &&
  isValidIssueYear iyr &&
  isValidExpirationYear eyr &&
  isValidHeight hgt &&
  isValidHairColor hcl &&
  isValidEyeColor ecl &&
  isValidPid pid

[<EntryPoint>]
let main _argv =
  let lines =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList
    
  let passports = ParseInput lines
  
  let part1Result = passports.Length
  let part2Result = passports |> List.filter isPassportValid |> List.length

  printfn "Part 1: %d" part1Result
  printfn "Part 2: %d" part2Result

  0
