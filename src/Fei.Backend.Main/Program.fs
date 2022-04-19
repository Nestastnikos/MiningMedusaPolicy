open System.IO
open Newtonsoft.Json
open VirtualSpaceTypes
open VirtualSpace
open System

type DtoSyscallInfoInput = { Name: string; Permissions: string; }
type SyscallInfoOutput = Map<string, VirtualSpacePermissions>

let syscallInfo =
  JsonConvert.DeserializeObject<DtoSyscallInfoInput list>
    ("./Fei.Backend.Main/Resources/syscall_config.json" |> File.ReadAllLines |> String.concat "")
  |> List.fold (fun acc x -> acc |> Map.add x.Name (convertToPermissions x.Permissions)) (Map([]))

let applicableEntries, unapplicableEntries =
  "./Fei.Backend.Main/Resources/full-mysqld_t.log"
  |> File.ReadAllLines
  |> (Audit.parseToAuditLogEntries >> Validation.validateAuditLogEntries)
  |> Seq.toList
  |> List.partition (fun x -> syscallInfo |> Map.containsKey x.Syscall)

let candidates =
  applicableEntries
  |> (PolicyMining.mineBasicRules syscallInfo >> PolicyMining.mergeRules >> PolicyMining.simplifyRules)

// printfn "%A" candidates
printfn "%A" unapplicableEntries