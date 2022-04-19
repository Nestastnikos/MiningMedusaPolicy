module Validation

open Audit
open Utils
open Types.CommonTypes
open System.Text.RegularExpressions
open System

type ModeType =
  | Directory
  | File
  | Character
  | Socket

type SElinuxContext = { User: string; Role: string; Type: string }
type ItemName = string

type ValidatedAuditEntry = {
  Id: int
  Proctitle: Proctitle
  Items: (ItemName * Nametype) list
  Mode: ModeType
  IsSticky: bool option
  Uid: Uid
}

let PATH_PATTERN = "^(/|./).*$"

let toMode input =
  match input with
  | "dir" -> Directory
  | "file" -> File
  | "character" -> Character
  | "socket" -> Socket
  | _ -> raise (ArgumentException "Unknown mode type")

let createId id =
  match id with
  | Some value ->
    match Regex.IsMatch (value, "(?<!-)[0-9]{1,8}") with
    | true ->
      int value
    | false ->
      raise (ArgumentException("Invalid ID - not a number"))
  | None ->
      raise (ArgumentException("Invalid ID - missing"))

let createProctitle proctitle =
  match proctitle with
  | Some value ->
    match Regex.IsMatch (value, "^/.+[^/]$") with
    | true ->
      value |> Proctitle
    | false ->
      raise (ArgumentException("Invalid proctitle - not in a format /some/path/to/file"))
  | None ->
      raise (ArgumentException("Invalid proctitle - missing"))

let createItems cwd itemIndexes itemNames itemNametypes =
  let cwdValue =
    match cwd with
    | Some value ->
      match Regex.IsMatch (value, "^(/|/.*[^/])$") with
      | true ->
        value
      | false ->
        raise (ArgumentException("Invalid cwd - incorrect path format"))
    | None ->
      raise (ArgumentException("Invalid cwd - missing"))

  let indexes =
    match itemIndexes with
    | Some values ->
      match values |> List.forall (fun x -> Regex.IsMatch (x, "[0-9]+")) with
      | true ->
        values |> List.map (fun x -> int x)
      | false ->
        raise (ArgumentException("Invalid item - index not a number"))
    | None ->
      raise (ArgumentException("Invalid item - missing index"))

  let isValidPath path = Regex.IsMatch(path, "^(/|./).*$") || Regex.IsMatch (path, "^[#\\-\\w.]*$")

  let castToNametype input =
    match input with
    | "PARENT" ->
      Parent
    | _ ->
      Other

  let createFullPath suffix =
    let result =
      match Regex.IsMatch (suffix, "^.+/$") with
      | true ->
        suffix |> StringUtils.skipLast 1 |> PathUtils.getCanonicalPath cwdValue
      | false ->
        PathUtils.getCanonicalPath cwdValue suffix
    result

  let names =
    match itemNames with
    | Some values ->
      match values |> List.filter (fun x -> not (isValidPath x)) with
      | [] ->
        values |> List.map (fun x -> x |> createFullPath)
      | unrecognized ->
        raise (ArgumentException("Invalid item - unrecognized name format - " + String.Join(",",unrecognized)))
    | None ->
      raise (ArgumentException("Invalid item - missing name"))

  let nametypes =
    match itemNametypes with
    | Some values ->
      values |> List.map (fun x -> castToNametype x)
    | None ->
      raise (ArgumentException("Invalid item - missing nametype"))

  match (indexes |> List.length) = (names |> List.length) && (names |> List.length) = (nametypes |> List.length) with
  | true ->
    List.zip3 indexes names nametypes
    |> List.sortBy (fun (i, _, _) -> int i)
    |> List.map (fun (_, x, y) -> (x, y))
  | false ->
    raise (ArgumentException("Invalid item - does not have index | name | nametype"))


let createModeTypeAndStickyBit input =
  match input with
  | Some (value: string) ->
      match value.Split "," with
      | [|mode; stickyness; _|] ->
        let mode = toMode mode
        let isSticky = stickyness.Equals("sticky", StringComparison.OrdinalIgnoreCase)
        match mode with
        | Directory ->
          (mode, Some(isSticky))
        | _ ->
          match isSticky with
          | false ->
            (mode, None)
          | true ->
            raise (ArgumentException("Invalid mode - nothing can be sticky except directory"))
      | [|mode; _|] ->
        let mode = toMode mode
        match mode with
        | Directory ->
          (mode, Some(false))
        | _ ->
          (mode, None)
      | _ ->
        raise (ArgumentException("Invalid mode - unknown format"))
  | None ->
      raise (ArgumentException("Invalid mode - missing"))


let createUid input =
  match input with
  | Some value ->
    match Regex.IsMatch (value, "[a-zA-Z]+") with
    | true ->
      value
    | false ->
      raise (ArgumentException("Invalid uid - does not contain only alpha chars"))
  | None ->
    raise (ArgumentException("Invalid uid - missing"))


let validateAuditLogEntries (entries: AuditLogRaw seq) =
  entries
  |> Seq.map (fun x ->
    let id = x.Id |> createId
    let uid = x.Uid |> createUid
    let proctitle = x.Proctitle |> createProctitle
    let (mode, isSticky) = x.Mode |> createModeTypeAndStickyBit
    let items = (x.ItemIndexes, x.ItemNames, x.ItemNametypes) |||> createItems x.CurrentWorkingDirectory
    { Id = id;
      Proctitle = proctitle
      Mode = mode;
      IsSticky = isSticky;
      Uid = uid;
      Items = items; })