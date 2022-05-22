open System.IO
open Newtonsoft.Json


let rules =
  JsonConvert.DeserializeObject<InputMapping.InputRuleDto list>("./test.json" |> File.ReadAllText)
  |> List.map (fun x -> InputMapping.ruleDtoToRule x)

let context = GrammarContext.createDefault rules
let result = GrammarContext.createLanguage context
("constable.conf", result) |> File.WriteAllText