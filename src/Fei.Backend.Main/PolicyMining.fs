module PolicyMining

open VirtualSpace.Types
open Validation
open Utils
open System
open Types.CommonTypes

let isRecursive nametype = nametype = Parent

let getNameForVirtualSpace pathEntries =
  let pathToVirtualSpaceName (path: string) =
    path.[1..].Replace ('/', '_')

  match pathEntries with
  | [first] ->
    match first.Path.FullPath with
    | "/" ->
      "all_files"
    | fullPath ->
      pathToVirtualSpaceName fullPath
  | paths when
      paths |> List.forall (fun x -> x.IsAddition) &&
      paths |> List.exists (fun x -> x.Path.Depth <> (List.head paths).Path.Depth) ->
    let parentCandidate = paths |> List.minBy (fun x -> x.Path.Depth)
    let isParent parent =
      paths |> List.except [parent]
      |> List.forall (fun x -> PathUtils.isPathParent parent.Path x.Path)
    match isParent parentCandidate with
    | true ->
      (pathToVirtualSpaceName parentCandidate.Path.FullPath) + "_with_children"
    | false ->
      let parentPath = parentCandidate.Path |> PathUtils.getParentPath |> CastUtils.optionToValueOrError
      (pathToVirtualSpaceName parentPath.FullPath) + "_with_children"
  | paths when
      paths |> List.forall (fun x -> x.Path.Depth = (List.head paths).Path.Depth) &&
      paths
        |> List.map (fun x -> PathUtils.getParentPath x.Path)
        |> ListUtils.areAllEqual ->
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
        |> List.forall (fun x -> x.IsAddition) &&
      paths
        |> List.sortBy (fun x -> x.Path.Depth)
        |> List.skip 1
        |> List.pairwise
        |> List.exists (fun (a,b) -> a <> b) ->
    let parent = paths |> List.sortBy (fun x -> x.Path.Depth) |> List.head
    (pathToVirtualSpaceName parent.Path.FullPath) + "_with_children_with_exceptions"
  | _ -> raise (NotImplementedException("Not implemented yet"))

let mergeFsVirtualSpacesIntoSingle virtualSpaces =
  virtualSpaces
  |> List.reduce (fun a b ->
    let newPaths = a.Paths @ b.Paths
    { Identifier = getNameForVirtualSpace newPaths; Paths = newPaths })

let mergeRules (rules: Rule list) =
  let rec mergeRulesRec rules depth =
    match depth with
    | 0 | 1 ->
      rules
    | currentDepth ->
      // First split the set of rules into two sets - the ones that we can possibly merge and the ones that we for sure cannot
      let ruleCandidates, unaffectedRules =
        rules
        |> List.partition (fun r -> r.Object.Paths |> List.map (fun p -> p.Path.Depth) |> List.forall (fun d -> d >= currentDepth))

      // Get the rules which contains only FS VS with current depth
      let currentDepthRules =
        ruleCandidates
        |> List.filter (fun r -> r.Object.Paths |> List.map (fun p -> p.Path.Depth) |> List.forall (fun d -> d = currentDepth))

      // Compute the parent paths for the currentDepthRules
      let groupingParentPaths =
        currentDepthRules
        |> List.map (fun x -> x.Object.Paths)
        |> ListUtils.flatten
        |> List.map (fun x -> PathUtils.getParentPath x.Path |> CastUtils.optionToValueOrError)
        |> List.distinct

      // Take all candidates and group them based on parentPaths, there can be possibly rules which do not match any path and these
      // rules are unmergeable. But those which could be grouped under some path can be merged into one.
      let mergeableRules, unmergeableRules =
        ruleCandidates
        |> List.groupBy
          (fun x ->
            groupingParentPaths
            |> List.tryPick
              (fun p -> if x.Object.Paths |> List.map (fun x -> x.Path) |> List.forall (fun cp -> PathUtils.isPathParent p cp)
                        then Some (p.FullPath) else None))
        |> List.partition (fun (path,_) -> path.IsSome)
        ||> ListUtils.mapForBoth (fun (_,x) -> x)

      let unmergedRules = unmergeableRules |> List.tryHead |> CastUtils.optionToList

      let mergedRules =
        mergeableRules
        |> List.map
          (fun rules ->
            let head = List.head rules
            {
              Subject = head.Subject;
              Object = rules |> List.map (fun x -> x.Object) |> mergeFsVirtualSpacesIntoSingle;
              Permissions = head.Permissions })
      mergeRulesRec (unaffectedRules @ mergedRules @ unmergedRules) (currentDepth-1)

  let maxDepth = (rules |> List.map (fun x -> x.Object.Paths) |> List.reduce (fun a b -> a @ b) |> List.maxBy (fun x -> x.Path.Depth)).Path.Depth
  mergeRulesRec (rules |> List.distinct) maxDepth

let simplifyVirtualSpace fsVs =
  let rec simplifyPathsRec paths currentDepth maxDepth =
    match currentDepth = maxDepth with
    | true ->
        paths
    | false ->
      let remainingPaths =
        paths
        |> List.filter (fun x -> x.Path.Depth = currentDepth && x.IsRecursive)
        |> List.fold
          (fun acc x ->
            acc |> List.filter (fun p ->
              not (PathUtils.isPathParent x.Path p.Path &&
              x.IsAddition = p.IsAddition)))
            paths
      simplifyPathsRec remainingPaths (currentDepth+1) maxDepth

  let depths = fsVs.Paths |> List.map (fun x -> x.Path.Depth) |> List.sort
  let (min, max) = (depths |> List.head, depths |> List.last)
  let result = simplifyPathsRec (fsVs.Paths) min max
  { Identifier = getNameForVirtualSpace result; Paths = result}

let simplifyRules rules =
  rules
  |> List.map (fun x -> {
    Subject = x.Subject;
    Object = simplifyVirtualSpace x.Object;
    Permissions = x.Permissions })


let mineBasicRules auditLogEntries =
  let rec mineBasicRulesRec uncoveredEntries rules =
    match Seq.isEmpty uncoveredEntries with
    | true -> rules
    | false ->
      let entry = Seq.head uncoveredEntries
      // TODO: change the implementation so it actually finds out based on syscall what
      // the VsPermissions should be. AllVsPermissions are now just placeholder.
      let newRules =
        entry.Items
        // use of AllVsPermissions is only temporary
        |> List.map (fun item -> { Uid=entry.Uid; Resource=item; Permissions=VirtualSpace.Constants.AllVsPermissions})
        |> List.map (fun x ->
          let (fullPath, nametype) = x.Resource
          // FIXME: we shouldnt do this option stuff here, it should be validated earlier
          let path = PathUtils.toPath fullPath
          let pathEntry = { Path = path; IsRecursive = isRecursive nametype; IsAddition = true; IsSticky = entry.IsSticky}
          let fsVs = { Identifier = fullPath; Paths = [pathEntry] }
          let processIdentifier = (entry.Uid, entry.Proctitle)
          { Subject = processIdentifier; Object = fsVs; Permissions = x.Permissions; })

      // TODO: Change this to actually filter only entries which's syscall requires the same VS permissions // as current entry
      let remainingEntries =
        uncoveredEntries
        |> Seq.filter (fun u ->
          not (u.Items |> List.forall
            (fun (path, _) ->
              rules |> List.exists (fun r -> (List.head r.Object.Paths).Path.FullPath = path &&
                                              r.Subject = (u.Uid, u.Proctitle)))))

      mineBasicRulesRec remainingEntries (rules |> List.append newRules)
  mineBasicRulesRec auditLogEntries []