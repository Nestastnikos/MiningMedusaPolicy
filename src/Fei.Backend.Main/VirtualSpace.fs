module VirtualSpace
    open System

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