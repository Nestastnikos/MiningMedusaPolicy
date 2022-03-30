module Audit
  open System
  open System.Text.RegularExpressions
  open Utils

  type ModeType =
    | Directory
    | File
    | Character
    | Socket

  [<FlagsAttribute>]
  type PermissionFlags = Read = 1 | Write = 2 | Execute = 4

  type SElinuxContext = { User: string; Role: string; Type: string }

  type AuditLog = {
    Id: int;
    Proctitle: string;
    Items: string list;
    Mode: ModeType * PermissionFlags[]
    Uid: string;
    SubjectContext: SElinuxContext
    ObjectContext: SElinuxContext;
    Syscall: string; // TODO: replace placeholder type
    Success: bool;
    ExitedWithFailure: bool;
    Command: string;
    ExecutedPath: string;
  }


  let toMode input =
    match input with
    | "dir" -> Directory
    | "file" -> File
    | "character" -> Character
    | "socket" -> Socket
    | _ -> raise (ArgumentException "Unknown mode type")


  let toPermissionFlags input =
      input
      |> Seq.map(fun x -> string x |> int)
      |> Seq.map (fun x -> enum<PermissionFlags>(x))
      |> Seq.toArray


  let castToModeAndPermissionFlags input =
    match input with
    | Some (x:string) ->
      match x.Split "," with
      | [|mode; permission|]
      | [|mode; _; permission|] ->
        (toMode mode),(toPermissionFlags permission)
      | _ -> raise (ArgumentException("Unknown mode format"))
    | None -> raise (ArgumentException("None cannot be casted"))


  let castToIsFailureCode code =
    match code with
    | Some value ->
      let pattern = "[0-9]+"
      not (Regex.IsMatch (value, pattern))
    | None -> raise (ArgumentException("None cannot be casted"))


  let castToSelinuxContext input =
    match input with
    | Some (value:string) ->
      match value.Split ":" with
      | [|seuser;serole;setype|]
      | [|seuser;serole;setype;_|] ->
        { User = seuser; Role = serole; Type = setype; }
      | _ -> raise (ArgumentException("Invalid field format"))
    | None -> raise (ArgumentException("None cannot be casted"))


  let castToIsSuccess input =
    match input with
    | Some (value:string) ->
      value.Equals("yes", StringComparison.Ordinal)
    | None -> raise (ArgumentException("None cannot be casted"))


  let toAuditLog (data: Map<string, string list>) =
    let getId data = data |> Map.find "id" |> ListUtils.fstValueOrNone |> CastUtils.optionToInt
    let getProctitle data = data |> Map.find "proctitle" |> ListUtils.fstValueOrNone |> CastUtils.optionToString
    let getItems data =
      let items = data |> Map.tryFind "item" |> CastUtils.optionToList |> List.map (fun x -> int x)
      let cwd = data |> Map.find "cwd" |> ListUtils.fstValueOrNone |> CastUtils.optionToString
      let names = data |> Map.tryFind "name" |> CastUtils.optionToList |> List.map (fun x -> PathUtils.getCanonicalPath cwd x)
      (List.zip items names) |> List.sortBy(fun (index, value) -> index) |> List.map (fun (index,value) -> value)

    let getMode data = data |> Map.find "mode" |> ListUtils.fstValueOrNone |> castToModeAndPermissionFlags
    let getExitedWithFailure data = data |> Map.find "exit" |> ListUtils.fstValueOrNone |> castToIsFailureCode
    let getUid data = data |> Map.find "uid" |> ListUtils.fstValueOrNone |> CastUtils.optionToString
    let getObjectContext data = data |> Map.find "obj" |> ListUtils.fstValueOrNone |> castToSelinuxContext
    let getSubjectContext data = data |> Map.find "subj" |> ListUtils.fstValueOrNone |> castToSelinuxContext
    let getSyscall data = data |> Map.find "syscall" |> ListUtils.fstValueOrNone |> CastUtils.optionToString
    let getIsSuccess data = data |> Map.find "success" |> ListUtils.fstValueOrNone |> castToIsSuccess
    let getCommand data = data |> Map.find "comm" |> ListUtils.fstValueOrNone |> CastUtils.optionToString
    let getExecutedPath data = data |> Map.find "exe" |> ListUtils.fstValueOrNone |> CastUtils.optionToString

    {
      Id = getId data;
      Proctitle = getProctitle data;
      Items = getItems data;
      Mode = getMode data;
      Uid = getUid data;
      ObjectContext = getObjectContext data;
      SubjectContext = getSubjectContext data;
      Syscall = getSyscall data;
      Success = getIsSuccess data;
      ExitedWithFailure = getExitedWithFailure data;
      Command = getCommand data;
      ExecutedPath = getExecutedPath data;
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