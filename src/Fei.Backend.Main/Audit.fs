module Audit
  open System
  open System.Text.RegularExpressions
  open Utils
  open Types.CommonTypes

  type AuditLogRaw = {
    Id: string option
    Proctitle: string option
    ItemIndexes: string list option
    ItemNames: string list option
    ItemNametypes: string list option
    Mode: string option
    Uid: string option
    CurrentWorkingDirectory: string option
    // SubjectContext: SElinuxContext
    // ObjectContext: SElinuxContext;
    // Syscall: string; // TODO: replace placeholder type
    // Success: bool;
    // ExitedWithFailure: bool;
    // Command: string;
    // ExecutedPath: string;
    }


  // let toPermissionFlags input =
  //     input
  //     |> Seq.map(fun x -> string x |> int)
  //     |> Seq.map (fun x -> enum<PermissionFlags>(x))
  //     |> Seq.toArray


  // let castToModeAndPermissionFlags input =
  //   match input with
  //   | Some (x:string) ->
  //     match x.Split "," with
  //     | [|mode; permission|]
  //     | [|mode; _; permission|] ->
  //       (toMode mode),(toPermissionFlags permission)
  //     | _ -> raise (ArgumentException("Unknown mode format"))
  //   | None -> raise (ArgumentException("None cannot be casted"))


  let castToIsFailureCode code =
    match code with
    | Some value ->
      let pattern = "[0-9]+"
      not (Regex.IsMatch (value, pattern))
    | None -> raise (ArgumentException("None cannot be casted"))


  // let castToSelinuxContext input =
  //   match input with
  //   | Some (value:string) ->
  //     match value.Split ":" with
  //     | [|seuser;serole;setype|]
  //     | [|seuser;serole;setype;_|] ->
  //       { User = seuser; Role = serole; Type = setype; }
  //     | _ -> raise (ArgumentException("Invalid field format"))
  //   | None -> raise (ArgumentException("None cannot be casted"))


  let castToIsSuccess input =
    match input with
    | None -> raise (ArgumentException("None cannot be casted"))
    | Some (value:string) ->
      value.Equals("yes", StringComparison.Ordinal)

  let castToNametype input =
    match input with
    | "PARENT" ->
      Nametype.Parent
    | _ ->
      Nametype.Other

  let toAuditLog (data: Map<string, string list>) =
    let getId () = data |> Map.tryFind "id" |> ListUtils.fstValueOrNone
    let getProctitle () = data |> Map.tryFind "proctitle" |> ListUtils.fstValueOrNone
    let getCurrentWorkingDirectory () = data |> Map.tryFind "cwd" |> ListUtils.fstValueOrNone
    let getItemIndexes () = data |> Map.tryFind "item"
    let getItemNames () = data |> Map.tryFind "name"
    let getItemNametypes () = data |> Map.tryFind "nametype"

    let getMode () = data |> Map.tryFind "mode" |> ListUtils.fstValueOrNone
    let getUid () = data |> Map.tryFind "uid" |> ListUtils.fstValueOrNone
    // let getExitedWithFailure () = data |> Map.tryFind "exit" |> ListUtils.fstValueOrNone
    // let getObjectContext () = data |> Map.find "obj" |> ListUtils.fstValueOrNone
    // let getSubjectContext () = data |> Map.find "subj" |> ListUtils.fstValueOrNone
    // let getSyscall () = data |> Map.find "syscall" |> ListUtils.fstValueOrNone
    // let getIsSuccess () = data |> Map.find "success" |> ListUtils.fstValueOrNone
    // let getCommand () = data |> Map.find "comm" |> ListUtils.fstValueOrNone
    // let getExecutedPath () = data |> Map.find "exe" |> ListUtils.fstValueOrNone

    {
      Id = getId ();
      Proctitle = getProctitle ();
      Mode = getMode ();
      Uid = getUid ();
      ItemIndexes = getItemIndexes ();
      ItemNames = getItemNames ();
      ItemNametypes = getItemNametypes ();
      CurrentWorkingDirectory = getCurrentWorkingDirectory ();

      // ObjectContext = getObjectContext data;
      // SubjectContext = getSubjectContext data;
      // Syscall = getSyscall data;
      // Success = getIsSuccess data;
      // ExitedWithFailure = getExitedWithFailure data;
      // Command = getCommand data;
      // ExecutedPath = getExecutedPath data;
    }


  let parseToAuditLogEntries source =
    let parseRawFields line =
      let pattern = "\\w+=[#\\w:\\/.,-]+(\\([\\w\\/\\s:.]+\\)){0,1}"
      Regex.Matches(line, pattern)

    let parseToMap (fields: MatchCollection) =
      fields
        |> Seq.map(fun x -> x.Value.Split "=")
        |> Seq.fold (fun (state:Map<string,string>) x -> state.Add (x.[0], x.[1])) (Map([]))

    let parseIdentifier field =
      let pattern = "(?<=:)[0-9]+(?=\\))"
      (Regex.Match (field, pattern)).Value

    source
    |> Seq.filter (fun (l:string) -> not (l.StartsWith "----"))
    |> Seq.map (fun l -> l |> (parseRawFields >> parseToMap))
    |> Seq.map (fun m -> m |> Map.add "id" (parseIdentifier m.["msg"]))
    |> Seq.groupBy (fun m -> m.["id"])
    |> Seq.map (fun (k,maps) -> maps |> MapUtils.concat)
    |> Seq.map (fun x -> toAuditLog x)