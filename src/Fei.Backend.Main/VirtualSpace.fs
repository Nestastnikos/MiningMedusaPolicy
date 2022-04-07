module VirtualSpace
  open System
  open CommonTypes

  let isRecursive nametype = nametype = Nametype.Parent

  module Types =
    [<FlagsAttribute>]
    type VirtualSpacePermissions = Read = 1 | Write = 2 | See = 4

    type PathEntry = {
      PathName: string;
      IsRecursive: bool;
      IsAddition: bool; }

    type VirtualSpaceFilesystem = {
      Identifier: string;
      Paths: PathEntry list; }

    type ProcessIdentifier = Uid * Proctitle

    type Rule = {
      Subject: ProcessIdentifier;
      Object: VirtualSpaceFilesystem;
      Permissions: VirtualSpacePermissions; }

    type Constraint = { Uid: Uid; Resource: (FullPath * Nametype); Permissions: VirtualSpacePermissions }


  module Constants =
    open Types

    let AllVsPermissions = VirtualSpacePermissions.Read ||| VirtualSpacePermissions.Write ||| VirtualSpacePermissions.See
