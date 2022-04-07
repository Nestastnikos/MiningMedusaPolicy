open System.IO
open VirtualSpace
open Audit


type VsCandidates = {
  Fs: VirtualSpaceFilesystem list;
  Processes: VirtualSpaceProcess list;
  Rules: Rule list; }

type Uid = string

type Constraint = { Uid: Uid; Resource: (string * Nametype); Permissions: VirtualSpacePermissions }

let isRecursive nametype = nametype = Nametype.Parent

let mineBasicRules auditLogEntries =
  let rec mineBasicRulesRec uncoveredEntries vsCandidates =
    match Seq.isEmpty uncoveredEntries with
    | true -> vsCandidates
    | false ->
      let entry = Seq.head uncoveredEntries
      // TODO: change the implementation so it actually finds out based on syscall what
      // the VsPermissions should be. AllVsPermissions are now just placeholder.
      let rules =
        entry.Items
        |> List.map (fun item -> { Uid=entry.Uid; Resource=item; Permissions=AllVsPermissions})
        |> List.map (fun x ->
          let (path, nametype) = x.Resource
          let pathEntry = { PathName = path; IsRecursive = isRecursive nametype; IsAddition = true; }
          let fsVs = { Identifier = x.Uid; Paths = [pathEntry] }
          let processVs = { Identifier = (entry.Uid, entry.Proctitle); }
          { Subject = processVs; Object = fsVs; Permissions = x.Permissions; })

      // cover similar entries
      // TODO: Change this to actually filter only entries which's syscall requires the same VS permissions
      // as current entry
      let remainingEntries =
        uncoveredEntries
        |> Seq.filter (fun u ->
          not (u.Items |> List.forall
            (fun (path, _) ->
              rules |> List.exists (fun r -> (List.head r.Object.Paths).PathName = path &&
                                              r.Subject.Identifier = (u.Uid, u.Proctitle)))))

      let newCandidates = {
        Fs = vsCandidates.Fs |> List.append (rules |> List.map (fun x -> x.Object));
        Processes = vsCandidates.Processes |> List.append (rules |> List.map (fun x -> x.Subject));
        Rules = vsCandidates.Rules |> List.append rules; }

      mineBasicRulesRec remainingEntries newCandidates
  mineBasicRulesRec auditLogEntries { Fs = []; Processes = []; Rules = []; }


// Consider whether "sticky" info is somehow useful to me
let candidates =
  "./Fei.Backend.Main/Resources/small-mysqld_t.log"
  |> File.ReadAllLines
  |> parseToAuditLogEntries
  |> mineBasicRules

printfn "%A" candidates.Fs