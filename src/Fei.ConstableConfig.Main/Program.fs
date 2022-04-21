open System.IO
open Newtonsoft.Json


let result =
  JsonConvert.DeserializeObject<InputMappingTypes.InputRuleDto list>("./test.json" |> File.ReadAllText)
  |> List.map (fun x -> InputMapping.ruleDtoToRule x)

printfn "%A" result