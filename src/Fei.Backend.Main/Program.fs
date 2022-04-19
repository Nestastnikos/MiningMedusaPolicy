open System.IO
open Newtonsoft.Json
open VirtualSpace
open VirtualSpaceTypes
open Validation

type SyscallInfoInputDto = { Name: string; Permissions: string; }
type SyscallInfo = Map<string, VirtualSpacePermissions>

let syscallInfo =
  JsonConvert.DeserializeObject<SyscallInfoInputDto list>
    ("./Fei.Backend.Main/Resources/syscall_config.json" |> File.ReadAllLines |> String.concat "")
  |> List.fold (fun acc x -> acc |> Map.add x.Name (convertToPermissions x.Permissions)) (Map([]))

let applicableEntries, unapplicableEntries =
  "./Fei.Backend.Main/Resources/fraction-mysqld_t.log"
  |> File.ReadAllLines
  |> (Audit.parseToAuditLogEntries >> Validation.validateAuditLogEntries)
  |> Seq.toList
  |> ProgramOutput.partitionBySyscall syscallInfo

let candidatesJson =
  applicableEntries
  |> (PolicyMining.mineBasicRules syscallInfo >> PolicyMining.mergeRules >> PolicyMining.simplifyRules)
  |> List.map (fun x -> ProgramOutput.mapToRuleDto x)
  |> JsonConvert.SerializeObject

File.WriteAllText ("test.json", candidatesJson);

// printfn "%A" candidatesJson
// printfn "%A" unapplicableEntries