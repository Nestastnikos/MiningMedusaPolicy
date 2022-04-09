module PolicyMining
  open VirtualSpace.Types
  open Utils
  open System

  let mergeRules (rules: Rule list) =
    rules

  let pathToVirtualSpaceName (path: string) =
    path.[1..].Replace ('/', '_')

  let getNameForVirtualSpace vsFs =
    match vsFs.Paths with
    | [first] ->
      match first.Path.FullPath with
      | "/" ->
        "all_files"
      | fullPath ->
        pathToVirtualSpaceName fullPath
    | paths when
        paths |> List.forall (fun x -> x.IsAddition) &&
        paths |> List.exists (fun x -> x.Path.Depth <> (List.head paths).Path.Depth) ->
      let parent = paths |> List.minBy (fun x -> x.Path.Depth)
      (pathToVirtualSpaceName parent.Path.FullPath) + "_with_children"
    | paths when
        paths |> List.forall (fun x -> x.Path.Depth = (List.head paths).Path.Depth) &&
        paths
          |> List.map (fun x -> PathUtils.getParentPath x.Path)
          |> List.pairwise
          |> List.forall (fun (x,y) -> x = y) ->
      let parent = PathUtils.getParentPath (paths |> List.head).Path |> CastUtils.optionToValueOrError
      pathToVirtualSpaceName parent.FullPath + "_some"
    | paths when
        paths |> List.exists (fun x -> x.Path.Depth <> (List.head paths).Path.Depth) &&
        paths
          |> List.sortBy (fun x -> x.Path.Depth)
          |> List.take 1
          |> List.forall (fun x -> x.IsRecursive && x.IsAddition) &&
        paths
          |> List.sortBy (fun x -> x.Path.Depth)
          |> List.skip 1
          |> List.forall (fun x -> not x.IsAddition) ->
      let parent = List.head paths
      (pathToVirtualSpaceName parent.Path.FullPath) + "_with_exceptions"
    | paths when
        paths
          |> List.sortBy (fun x -> x.Path.Depth)
          |> List.take 1
          |> List.forall (fun x -> x.IsRecursive && x.IsAddition) &&
        paths
          |> List.sortBy (fun x -> x.Path.Depth)
          |> List.skip 1
          |> List.pairwise
          |> List.exists (fun (a,b) -> a <> b) ->
      let parent = paths |> List.sortBy (fun x -> x.Path.Depth) |> List.head
      (pathToVirtualSpaceName parent.Path.FullPath) + "_with_children_with_exceptions"
    | _ -> raise (NotImplementedException("Not implemented yet"))
