open System.IO
open Newtonsoft.Json
open VirtualSpace
open VirtualSpaceTypes
open System

type SyscallInfoInputDto = { Name: string; Permissions: string; }
type SyscallInfo = Map<string, VirtualSpacePermissions>

[<EntryPoint>]
let main (args) =
  let (input, output) =
    match args with
    | [||] ->
      ("./Fei.Backend.Main/Resources/full-mysqld_t.log", "policy.json")
    | [|first; second|] ->
      (first, second)
    | _ ->
      raise (ArgumentException("Invalid number of command line arguments"))

  let syscallInfo =
    JsonConvert.DeserializeObject<SyscallInfoInputDto list>
      ("./Fei.Backend.Main/Resources/syscall_config.json" |> File.ReadAllLines |> String.concat "")
    |> List.fold (fun acc x -> acc |> Map.add x.Name (convertToPermissions x.Permissions)) (Map([]))

  let applicableEntries, unapplicableEntries =
    input
    |> File.ReadAllLines
    |> (Audit.parseToAuditLogEntries >> Validation.validateAuditLogEntries)
    |> Seq.toList
    |> ProgramOutput.partitionBySyscall syscallInfo

  let candidatesJson =
    applicableEntries
    |> (PolicyMining.mineBasicRules syscallInfo >> PolicyMining.mergeRules >> PolicyMining.simplifyRules)
    |> List.map (fun x -> ProgramOutput.mapToRuleDto x)
    |> JsonConvert.SerializeObject

  File.WriteAllText (output, candidatesJson);
  0