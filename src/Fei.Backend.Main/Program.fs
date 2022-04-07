open System.IO
open VirtualSpace
open Audit


type VsCandidates = {
  Fs: VirtualSpaceFilesystem list;
  Processes: VirtualSpaceProcess list;
  Rules: Rule list; }

type Constraint = { Path: string; Permissions: VirtualSpacePermissions }


let mineBasicRules auditLogEntries =
  let rec mineBasicRulesRec uncoveredEntries vsCandidates =
    match Seq.isEmpty uncoveredEntries with
    | true -> vsCandidates
    | false ->
      let entry = Seq.head uncoveredEntries

      // TODO: change the implementation so it actually finds out based on syscall what
      // the VsPermissions should be. AllVsPermissions are now just placeholder.
      let (subject, object, vsPermissions) = (entry.Uid, List.last entry.Items, AllVsPermissions)

      // TODO: For now we keep it as simple as possible, this step si not necessary, we can allow duplicates
      // let subjectsAccessingResource =
      //   uncoveredEntries
      //   |> Seq.filter (fun x -> x.Proctitle = path)
      //   |> Seq.map ( fun x -> x.SubjectContext)
      //   |> Seq.append subject
      //   |> Seq.distinct

      let pathEntry = { PathName = object; IsRecursive = false; IsAddition = true; }
      let fsVs = { Name = object; Paths = [pathEntry] }

      let processVs = {
        Name = subject;
        EnteredTree = (String.concat "/" ["domain"; subject], false); }

      let rule = { Subject = processVs; Object = fsVs; Permissions = vsPermissions; }

      // cover similar entries
      // TODO: Change this to actually filter only entries which's syscall requires the same VS permissions
      // as current entry
      // TODO: this produces incorrect behavior when we filter based on Proctitle
      let remainingEntries =
        uncoveredEntries |> Seq.filter (fun x -> not (List.last x.Items = fsVs.Name && x.Uid = subject))

      let newCandidates = {
        Fs = vsCandidates.Fs |> List.append [fsVs];
        Processes = vsCandidates.Processes |> List.append [processVs];
        Rules = vsCandidates.Rules |> List.append [rule]; }

      mineBasicRulesRec remainingEntries newCandidates
  mineBasicRulesRec auditLogEntries { Fs = []; Processes = []; Rules = []; }


// Consider whether "sticky" info is somehow useful to me
let candidates =
  "./Fei.Backend.Main/Resources/fraction-mysqld_t.log"
  |> File.ReadAllLines
  |> parseToAuditLogEntries
  |> mineBasicRules

printfn "%A" candidates