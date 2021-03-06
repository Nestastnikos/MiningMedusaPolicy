module Utils

  module MapUtils =
    open System
    let concat maps =
      maps
      |> Seq.fold
        (fun (state:Map<'a,'b list>) map ->
          Map.fold (fun acc key value ->
            match Map.containsKey key acc with
            | true ->
              let prev = Map.find key acc
              Map.add key (prev @ [value]) acc
            | false ->
              Map.add key [value] acc) state map)
          (Map([]))

    let getFstOrThrow key map =
      match Map.find key map with
      | value::tail ->
        value
      | _ -> raise (ArgumentException("The key is not present in the map"))

  module ListUtils =
    let fstValueOrNone list =
      match list with
      | Some values ->
        match values with
        | first::_->
          Some(first)
        | _ -> None
      | None -> None

    let areAllEqual list  =
      list
      |> List.pairwise
      |> List.forall (fun (x,y) -> x = y)

    let flatten listOfLists =
      listOfLists
      |> List.fold (fun acc x -> acc @ x) []

    let mapForBoth fnc list1 list2 =
      (list1 |> List.map fnc, list2 |> List.map fnc)

  module CastUtils =
    open System
    let optionToString option =
      match option with
      | Some (x:string) ->
        x
      | None -> raise (ArgumentException("Invalid cast"))

    let optionToInt option =
      optionToString option |> int

    let optionToList option =
      match option with
      | Some (x:'a list) ->
        x
      | None -> []

    let optionToValueOrError option =
      match option with
      | Some x ->
        x
      | None -> raise (ArgumentException("Invalid cast"))

  module StringUtils =
    let head (input:string) = input.[0]
    let skipLast (count: int) (input: string) =
      let length = String.length input
      input.[.. length - count - 1]


  module PathUtils =
    open Types.CommonTypes
    open System.Text.RegularExpressions
    open System

    let getCanonicalPath cwd targetPath =
      match StringUtils.head targetPath with
      | '.' when targetPath = "." ->
        cwd
      | '.' -> //relative path
        match String.length cwd with
          | 1 ->
            String.concat "" [ cwd;targetPath.[2..] ]
          | _ ->
            String.concat "/" [ cwd;targetPath.[2..] ]
      | '/' -> // absolute path
          targetPath
      | _ -> // only the name of the directory
        match String.length cwd with
        | 1 -> // this means that the cwd contains only /
          String.concat "" [ cwd;targetPath ]
        | _ ->
          String.concat "/" [ cwd;targetPath ]

    let tryToPath input =
      match StringUtils.head input with
      | '/' ->
        let segments =
          input.[1..].Split "/"
          |> Array.toList
          |> List.append ["/"]
          |> List.filter (fun x -> not (x.Length = 0))
        Some { FullPath = input; Segments = segments; Depth = segments.Length-1; }
      | _ ->
        None

    let toPath input =
      match StringUtils.head input with
      | '/' ->
        let segments =
          input.[1..].Split "/"
          |> Array.toList
          |> List.append ["/"]
          |> List.filter (fun x -> not (x.Length = 0))
        { FullPath = input; Segments = segments; Depth = segments.Length-1; }
      | _ ->
        raise (ArgumentException("The path does not have parent - " + input))

    let getParentPath path =
      match List.rev path.Segments with
      | [] | [_] ->
        None
      | first::tail ->
        let fullPath = (List.rev tail |> String.concat "/").[1..]
        tryToPath fullPath

    let isPathParent parentCandidate testedPath =
      Regex.IsMatch(testedPath.FullPath, parentCandidate.FullPath + "+.")