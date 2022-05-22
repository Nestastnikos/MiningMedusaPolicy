open System.IO
open Newtonsoft.Json
open System

[<EntryPoint>]
let main (args) =
  let (input, output) =
    match args with
    | [||] ->
      ("policy.json", "constable.conf")
    | [|first; second|] ->
      (first, second)
    | _ ->
      raise (ArgumentException("Invalid number of command line arguments"))

  let rules =
    JsonConvert.DeserializeObject<InputMapping.InputRuleDto list>(input |> File.ReadAllText)
    |> List.map (fun x -> InputMapping.ruleDtoToRule x)

  let context = GrammarContext.createDefault rules
  let result = GrammarContext.createLanguage context
  (output, result) |> File.WriteAllText
  0