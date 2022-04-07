module VirtualSpace
    open System

    type IsRecursive = bool

    [<FlagsAttribute>]
    type VirtualSpacePermissions = Read = 1 | Write = 2 | See = 4

    let AllVsPermissions = VirtualSpacePermissions.Read ||| VirtualSpacePermissions.Write ||| VirtualSpacePermissions.See

    type FullPath = FullPath of string

    type PathEntry = {
      PathName: string;
      IsRecursive: bool;
      IsAddition: bool; }

    type VirtualSpaceFilesystem = {
      Identifier: string;
      Paths: PathEntry list; }

    type VirtualSpaceProcess = { Identifier: string * string; }

    type Rule = {
      Subject: VirtualSpaceProcess;
      Object: VirtualSpaceFilesystem;
      Permissions: VirtualSpacePermissions; }
