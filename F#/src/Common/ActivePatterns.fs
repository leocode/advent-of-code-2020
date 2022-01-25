namespace Common

open System
open System.Text.RegularExpressions

module ActivePatterns =
  let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None
    
  let (|Integer|_|) (str: string) =
    let mutable intValue = 0
    if Int32.TryParse(str, &intValue) then Some(intValue)
    else None

