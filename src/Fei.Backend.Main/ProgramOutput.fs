module ProgramOutput

open VirtualSpaceTypes
open Validation

type ErrorAuditEntryDto = { EntryId: int; Message: string }

type ResultDto = {
  MinedRules: Rule list;
  SkippedEntries: ValidatedAuditEntry list;
}

type AbstractPathEntry(fullPath: string, isAddition: bool, fileType: string) =
  member this.FullPath = fullPath
  member this.IsAddition = isAddition
  member this.Type = fileType

type FileDto(fullPath: string, isAddition: bool) =
  inherit AbstractPathEntry(fullPath, isAddition, "FILE")


type DirectoryDto(fullPath: string, isAddition: bool, isRecursive: bool, isSticky: bool) =
  inherit AbstractPathEntry(fullPath, isAddition, "DIR")

  member this.IsRecursive = isRecursive
  member this.IsSticky = isSticky

type VirtualSpaceFsDto = {
  Identifier: string;
  Paths: AbstractPathEntry list;
}

type RuleDto = {
  SubjectUid: string;
  SubjectProctitle: string;
  Object: VirtualSpaceFsDto
  Permissions: string
}

let mapToRuleDto (rule: Rule) =
  let uid, proctitle = rule.Subject
  let paths =
    rule.Object.Paths
    |> List.map (fun x ->
        let result: AbstractPathEntry =
          match x.IsSticky.IsSome with
          | true ->
            DirectoryDto(x.Path.FullPath, x.IsAddition, x.IsRecursive, x.IsSticky.Value)
          | false ->
            FileDto(x.Path.FullPath, x.IsAddition)
        result)
  { SubjectUid = uid;
    SubjectProctitle = proctitle;
    Object = { Identifier = rule.Object.Identifier; Paths = paths };
    Permissions = VirtualSpace.permissionsToString rule.Permissions }

let partitionBySyscall syscallInfo entries =
  let valid, invalid =
    entries
    |> List.partition (fun x -> syscallInfo |> Map.containsKey x.Syscall)

  (valid, invalid
          |> List.map (fun x -> { EntryId = x.Id; Message = "Unknown syscall - " + x.Syscall}))
