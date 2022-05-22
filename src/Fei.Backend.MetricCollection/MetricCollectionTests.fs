module Fei.Backend.MetricCollectionTests

open NUnit.Framework
open VirtualSpace
open VirtualSpaceTypes
open Newtonsoft.Json
open System.IO
open System

type SyscallInfoInputDto = { Name: string; Permissions: string; }
type SyscallInfo = Map<string, VirtualSpacePermissions>


[<SetUp>]
let Setup () =
    ()

[<Test>]
[<Ignore("Not needed now")>]
let CollectMetricsForMysqlServerPolicyMining() =
    let syscallInfo =
      JsonConvert.DeserializeObject<SyscallInfoInputDto list>
        ("../../../Resources/syscall_config.json" |> File.ReadAllLines |> String.concat "")
      |> List.fold (fun acc x -> acc |> Map.add x.Name (convertToPermissions x.Permissions)) (Map([]))

    let applicableEntries, _ =
      "../../../Resources/full-mysqld_t.log"
      |> File.ReadAllLines
      |> (Audit.parseToAuditLogEntries >> Validation.validateAuditLogEntries)
      |> Seq.toList
      |> ProgramOutput.partitionBySyscall syscallInfo

    let rulesInitial =
      applicableEntries
      |> PolicyMining.mineBasicRules syscallInfo

    let rulesMerged =
      rulesInitial
      |> PolicyMining.mergeRules

    let rulesSimplified =
      rulesMerged
      |> PolicyMining.simplifyRules

    let computeMetrics = MetricOutput.setupMetricComputation 0.5 1.0 0.75
    let resultMetrics = [ (applicableEntries |> List.length, "INITIAL", rulesInitial) |||> computeMetrics;
      (applicableEntries |> List.length, "MERGED", rulesMerged) |||> computeMetrics;
      (applicableEntries |> List.length, "FINAL", rulesSimplified) |||> computeMetrics; ]

    File.WriteAllText ("../../../Output/out.json", resultMetrics |> JsonConvert.SerializeObject);

[<Test>]
let ``Collect metrics from policy mining for various number of log entries``() =
    let syscallInfo =
      JsonConvert.DeserializeObject<SyscallInfoInputDto list>
        ("../../../Resources/syscall_config.json" |> File.ReadAllLines |> String.concat "")
      |> List.fold (fun acc x -> acc |> Map.add x.Name (convertToPermissions x.Permissions)) (Map([]))

    let applicableEntries, _ =
      "../../../Resources/full-mysqld_t.log"
      |> File.ReadAllLines
      |> (Audit.parseToAuditLogEntries >> Validation.validateAuditLogEntries)
      |> Seq.toList
      |> ProgramOutput.partitionBySyscall syscallInfo

    let computeMetrics = MetricOutput.setupMetricComputation 0.5 1.0 0.75

    let mineRules numEntries =
      let rulesInitial =
        applicableEntries
        |> List.take numEntries
        |> PolicyMining.mineBasicRules syscallInfo

      let rulesMerged =
        rulesInitial
        |> PolicyMining.mergeRules

      let rulesSimplified =
        rulesMerged
        |> PolicyMining.simplifyRules
      [ (numEntries, "INITIAL", rulesInitial) |||> computeMetrics;
        (numEntries, "MERGED", rulesMerged) |||> computeMetrics;
        (numEntries, "FINAL", rulesSimplified) |||> computeMetrics; ]
    let resultMetrics =
      { 1 .. 30 .. (applicableEntries |> List.length) }
      |> Seq.append <| Seq.singleton (applicableEntries |> List.length)
      |> Seq.map (fun x -> mineRules x)
      |> Seq.concat
      |> Seq.toList

    let fileName = "metrics-" + DateTime.Now.ToString("yyyy-MM-dd-hh-mm-ss") + ".json"

    File.WriteAllText ("../../../Output/" + fileName, resultMetrics |> JsonConvert.SerializeObject);
