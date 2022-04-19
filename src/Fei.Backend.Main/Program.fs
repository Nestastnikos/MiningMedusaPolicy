open System.IO

let candidates =
  "./Fei.Backend.Main/Resources/full-mysqld_t.log"
  |> File.ReadAllLines
  |> (Audit.parseToAuditLogEntries >> Validation.validateAuditLogEntries)
  |> (PolicyMining.mineBasicRules >> PolicyMining.mergeRules >> PolicyMining.simplifyRules)

printfn "%A" candidates