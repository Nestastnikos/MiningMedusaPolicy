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

type PathItem = {
  Name: string;
  Nametype: Nametype;
  IsSticky: bool option;
  Mode: ModeType; }

type ValidatedAuditEntry = {
  Id: int
  Proctitle: Proctitle
  Items: PathItem list
  Uid: Uid
  Syscall: string }

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

let createItems cwd itemNames itemNametypes itemModes =
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

  let createModeTypeAndStickyBit (input: string) =
    match input.Split "," with
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


  let modesWithStickyBits =
    match itemModes with
    | Some values ->
      values |> List.map (fun x -> createModeTypeAndStickyBit x)
    | None ->
      raise (ArgumentException("Invalid mode - missing"))

  match (names |> List.length) = (nametypes |> List.length) && (names |> List.length) = (modesWithStickyBits |> List.length) with
  | true ->
    { 0 .. ((names |> List.length) - 1)  }
    |> Seq.map (fun x ->
      let name = names |> List.item x
      let (mode,isSticky) = modesWithStickyBits |> List.item x
      let nametype = nametypes |> List.item x
      { Name = name; Mode = mode; IsSticky = isSticky; Nametype = nametype })
    |> Seq.toList
  | false ->
    raise (ArgumentException("Invalid item - does not have index | name | nametype | mode"))


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

let createSyscall input =
  match input with
  | Some value ->
    match Regex.IsMatch (value, "\\w+") with
    | true ->
      value
    | false ->
      raise (ArgumentException("Invalid syscall - can contain only alphanumeric characters"))
  | None ->
    raise (ArgumentException("Invalid syscall - missing"))



let validateAuditLogEntries (entries: AuditLogRaw seq) =
  entries
  |> Seq.map (fun x ->
    let id = x.Id |> createId
    let uid = x.Uid |> createUid
    let proctitle = x.Proctitle |> createProctitle
    let items = (x.ItemNames, x.ItemNametypes, x.Mode) |||> createItems x.CurrentWorkingDirectory
    let syscall = x.Syscall |> createSyscall
    { Id = id;
      Proctitle = proctitle
      Uid = uid;
      Items = items;
      Syscall = syscall; })