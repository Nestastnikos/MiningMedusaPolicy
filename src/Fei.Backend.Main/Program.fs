open System.IO
open Audit


"./Fei.Backend.Main/Resources/example-mysqld_t.log"
  |> File.ReadAllLines
  |> parseToAuditLogEntries
  |> Seq.iter (printfn "%A")