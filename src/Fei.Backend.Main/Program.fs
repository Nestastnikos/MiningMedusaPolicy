open System.IO
open System
open Audit

type IsRecursive = bool


[<FlagsAttribute>]
type VirtualSpacePermissions = Read = 1 | Write = 2 | See = 4

let AllVsPermissions = VirtualSpacePermissions.Read ||| VirtualSpacePermissions.Write ||| VirtualSpacePermissions.See


type PathEntry = {
  PathName: string;
  IsRecursive: bool;
  IsAddition: bool;
}


type VirtualSpaceFilesystem = {
  Name: string;
  Paths: PathEntry list
}


type VirtualSpaceProcess = {
  Name: string;
  EnteredTree: string * IsRecursive;
  VirtualSpaces: (VirtualSpaceFilesystem * VirtualSpacePermissions) list;
}

let allFilesEntry = { PathName = "/"; IsRecursive = true; IsAddition = true; }
let allFilesVs = { Name = "all_files"; Paths = List.singleton allFilesEntry}
let allProcessesVs = {
  Name = "all_processes";
  EnteredTree = ("domain",true);
  VirtualSpaces = List.singleton (allFilesVs, AllVsPermissions)
  }

"./Fei.Backend.Main/Resources/example-mysqld_t.log"
  |> File.ReadAllLines
  |> parseToAuditLogEntries
  |> Seq.iter (printfn "%A")

printfn "%A" allProcessesVs