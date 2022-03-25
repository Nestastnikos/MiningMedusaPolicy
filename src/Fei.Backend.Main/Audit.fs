module Audit
  open System
  open System.Text.RegularExpressions

  type ModeType =
    | Directory
    | File

  [<FlagsAttribute>]
  type PermissionFlags = Read = 1 | Write = 2 | Execute = 4

  type SElinuxContext = { User: string; Role: string; Type: string }

  type AuditLog = {
    Id: int;
    Proctitle: string;
    Items: (int * string)[];
    Mode: ModeType * PermissionFlags[]
    Uid: string;
    SubjectContext: SElinuxContext
    ObjectContext: SElinuxContext;
    CurrentWorkingDirectory: string;
    Syscall: string; // TODO: replace placeholder type
    Success: bool;
    ExitedWithFailure: bool;
    Command: string;
    ExecutedPath: string;
  }


  let maptoAuditLog (data: Map<string, string>) =
    let parseModeAndPermissionFlags (input:string) =
      let modeAndPermission = input.Split ","
      let mode =
        match modeAndPermission.[0] with
        | "dir" -> Directory
        | "file" -> File
        | _ -> raise (ArgumentException "Unknown mode type")
      let permissionFlags =
        modeAndPermission.[1]
        |> Seq.map(fun x -> string x |> int)
        |> Seq.map (fun x -> enum<PermissionFlags>(x))
        |> Seq.toArray
      mode, permissionFlags

    let parseSelinuxContext (input: string) =
      let context = input.Split ":"
      { User = context.[0]; Role = context.[1]; Type = context.[2]; }


    let isFailureCode code =
      let pattern = "[0-9]+"
      not (Regex.IsMatch (code, pattern))

    {
      Id = int data.["id"];
      Proctitle = data.["proctitle"];
      Items = [|int data.["item"], data.["name"] |]
      Mode = parseModeAndPermissionFlags data.["mode"];
      Uid = data.["uid"];
      ObjectContext = parseSelinuxContext data.["obj"];
      SubjectContext = parseSelinuxContext data.["subj"];
      CurrentWorkingDirectory = data.["cwd"];
      Syscall = data.["syscall"];
      Success = data.["success"].Equals("yes", StringComparison.OrdinalIgnoreCase);
      ExitedWithFailure = isFailureCode data.["exit"]
      Command = data.["comm"];
      ExecutedPath = data.["exe"];
    }

  let parseToAuditLogEntries source =
    let parseRawFields line =
      let pattern = "\\w+=[\\w:\\/.,]+(\\([\\w\\/\\s:.]+\\)){0,1}"
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
    |> Seq.map (fun (k,maps) -> maps |> Utils.merge)
    |> Seq.map (fun x -> maptoAuditLog x)