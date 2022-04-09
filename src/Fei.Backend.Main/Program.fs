open System.IO
open Audit
open Utils
open VirtualSpace
open VirtualSpace.Types
open VirtualSpace.Constants


let mineBasicRules auditLogEntries =
  let rec mineBasicRulesRec uncoveredEntries rules =
    match Seq.isEmpty uncoveredEntries with
    | true -> rules
    | false ->
      let entry = Seq.head uncoveredEntries
      // TODO: change the implementation so it actually finds out based on syscall what
      // the VsPermissions should be. AllVsPermissions are now just placeholder.
      let newRules =
        entry.Items
        |> List.map (fun item -> { Uid=entry.Uid; Resource=item; Permissions=AllVsPermissions})
        |> List.map (fun x ->
          let (fullPath, nametype) = x.Resource
          // FIXME: we shouldnt do this option stuff here, it should be validated earlier
          let path = CastUtils.optionToValueOrError (PathUtils.toPath fullPath)
          let pathEntry = { Path = path; IsRecursive = isRecursive nametype; IsAddition = true; }
          let fsVs = { Identifier = fullPath; Paths = [pathEntry] }
          let processIdentifier = (entry.Uid, entry.Proctitle)
          { Subject = processIdentifier; Object = fsVs; Permissions = x.Permissions; })

      // TODO: Change this to actually filter only entries which's syscall requires the same VS permissions // as current entry
      let remainingEntries =
        uncoveredEntries
        |> Seq.filter (fun u ->
          not (u.Items |> List.forall
            (fun (path, _) ->
              rules |> List.exists (fun r -> (List.head r.Object.Paths).Path.FullPath = path &&
                                              r.Subject = (u.Uid, u.Proctitle)))))

      mineBasicRulesRec remainingEntries (rules |> List.append newRules)
  mineBasicRulesRec auditLogEntries []


// Consider whether "sticky" info is somehow useful to me
let candidates =
  "./Fei.Backend.Main/Resources/full-mysqld_t.log"
  |> File.ReadAllLines
  |> parseToAuditLogEntries
  |> mineBasicRules

printfn "%A" candidates