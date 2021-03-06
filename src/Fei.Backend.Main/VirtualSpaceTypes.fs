module VirtualSpaceTypes
open System
open Types.CommonTypes
open Validation

[<FlagsAttribute>]
type VirtualSpacePermissions = Read = 1 | Write = 2 | See = 4

type PathEntry = {
  Path: Path;
  IsRecursive: bool;
  IsAddition: bool;
  IsSticky: bool option;
  IsDirectory: bool; }

type VirtualSpaceFilesystem = {
  Identifier: string;
  Paths: PathEntry list; }

type Rule = {
  Subject: ProcessIdentifier;
  Object: VirtualSpaceFilesystem;
  Permissions: VirtualSpacePermissions; }

type Constraint = { Uid: Uid; Resource: PathItem; Permissions: VirtualSpacePermissions }


module Constants =
  let AllVsPermissions = VirtualSpacePermissions.Read ||| VirtualSpacePermissions.Write ||| VirtualSpacePermissions.See
