module GrammarContext


open DomainTypes
open System


type T = {
  FsTreeName: string;
  ProcessTreeName: string;
  Rules: Rule list; }


let create fsTreeName processTreeName rules =
  { FsTreeName = fsTreeName;
    ProcessTreeName = processTreeName;
    Rules = rules; }

let createDefault uncoveredRules =
  create "fs" "domains" uncoveredRules


let derivateProcDef context =
  context.Rules
  |> List.map (fun x -> x.Subject)
  |> List.distinct
  |> List.map (fun (uid, _) -> ["\n"; ";"; "space " + uid + " = " + context.ProcessTreeName + "/" + uid])
  |> List.concat

let derivateProcInits context =
  context.Rules
  |> List.map (fun x -> x.Subject)
  |> List.distinct
  |> List.map (fun (uid, _) ->
    ["\n"; ";"; uid + "\tENTER " + uid + ", READ " + uid + ", WRITE " + uid + ", SEE " + uid])
  |> List.concat

let derivateVsDef context =
  let getSignOperator isAddition =
    match isAddition with
    | true -> "+"
    | false -> "-"

  let derivatePath trimSign path =
    let fullPath =
      match path with
      | File f ->
        String.Join (" ", [getSignOperator f.IsAddition; f.FullPath])
      | Directory d ->
        let fullPath =
          match d.IsRecursive with
          | true ->
            "recursive " + d.FullPath
          | false ->
            d.FullPath
        String.Join (" ", [getSignOperator d.IsAddition; fullPath])

    match trimSign with
    | true ->
      fullPath.[1..].TrimStart()
    | false ->
      fullPath

  let derivateNextPath path =
    derivatePath false path

  context.Rules
  |> List.map (fun x -> x.Object)
  |> List.map (fun (id, paths) ->
    List.append
      ["space " + id + " = " + derivatePath true (paths |> List.head)]
      (paths
      |> List.skip 1
      |> List.map (fun p -> ["\n\t\t"; derivateNextPath p])
      |> List.concat
      |> List.append <| [";"; "\n"]))
  |> List.concat
  |> List.rev

let derivateRules context =
  let derivatePermissions  id permissions =
    let outPermissions =
      permissions
      |> List.map (fun x ->
        let permission =
          match x with
          | Read -> "READ"
          | Write -> "WRITE"
          | See -> "SEE"
        String.Join (" ", [permission; id]))
    String.Join (", ", outPermissions)

  context.Rules
  |> List.map (fun x ->
    let (uid, _) = x.Subject
    let (id, _) = x.Object
    ["\n"; ";"; derivatePermissions id x.Permissions; "\t"; uid])
  |> List.concat

let derivateGetProcess context =
  let requiresBranching =
    match context.Rules |> List.length with
    | 0 -> false
    | _ -> true

  let derivateBranches context nonTerminal =
    match requiresBranching with
    | false ->
      let terminal = "enter(process,@\"<<id>>/init\");".Replace("<<id>>", context.ProcessTreeName)
      [terminal]
    | true ->
      context.Rules
      |> List.map (fun x -> x.Subject)
      |> List.distinct
      |> List.mapi (fun i (uid, proctitle) ->
        let prefix =
          match i with
          | 0 -> "if"
          | _ -> "elseif"

        let terminal = prefix + " (proctitle.cmdline == \"<<proctitle>>\") {\n\t\tenter(process,@\"<<domainName>>/<<uid>>\");\n\t}"
        terminal
          .Replace("<<proctitle>>", proctitle)
          .Replace("<<domainName>>", context.ProcessTreeName)
          .Replace("<<uid>>", uid))
      |> List.append <| [" else {\n\t\tenter(process,@\"<<domainName>>/init\");\n\t}\n\t".Replace("<<domainName>>", context.ProcessTreeName)]
      |> List.rev
  ["\n"; "}"; "\n"; "return OK;"] @ derivateBranches context requiresBranching @ ["\n\t"; "{"; "\n"; "* getProcess *"]

let derivate context nonterminal =
  match nonterminal with
  | "<start>" ->
    ([], ["<autogenerated_header>"; "<file_tree>"; "<newl>"; "<process_tree>"; "<newl>"; "<newl>"; "<vs_section_comment>"; "<vs_defs>"; "<newl>"; "<newl>"; "<rule_section_comment>"; "<rules>"; "<newl>"; "<newl>"; "<functions>"])
  | "<autogenerated_header>" ->
    (["\n"; "// any manual changes are lost when the file is pregenerated"; "\n"; "// autogenerated configuration file"], [])
  | "<file_tree>" ->
    ([], ["<file_tree_def>"; "<newl>"; "<is_primary_tree>"])
  | "<file_tree_def>" ->
    let terminal = "tree \"<<id>>\" clone of file by getfile getfile.filename;".Replace ("<<id>>", context.FsTreeName)
    ([terminal], [])
  | "<is_primary_tree>" ->
    let terminal = "primary tree \"<<id>>\";".Replace ("<<id>>", context.FsTreeName)
    ([terminal], [])
  | "<process_tree>" ->
    ([], ["<process_tree_def>"])
  | "<process_tree_def>" ->
    let terminal = "tree \"<<id>>\" of process;".Replace ("<<id>>", context.ProcessTreeName)
    ([terminal], [])
  | "<vs_section_comment>" ->
    (["\n"; "// virtual space definitions"], [])
  | "<vs_defs>" ->
    ([], ["<vs_fs_defs>"; "<newl>"; "<vs_proc_defs>"; "<newl>"; "<vs_proc_inits>"])
  | "<vs_fs_defs>" ->
    (derivateVsDef context, [])
  | "<vs_proc_defs>" ->
    (derivateProcDef context, [])
  | "<vs_proc_inits>" ->
    (derivateProcInits context, [])
  | "<rule_section_comment>" ->
    (["\n"; "// rule definitions"], [])
  | "<rules>" ->
    (derivateRules context, [])
  | "<functions>" ->
    (["\n"; "}"; "{"; "\n"; "function constable_init"; "\n"; "}"; "{"; "\n"; "function init"], ["<enter_domain>"; "<newl>"; "<get_process>"; "<newl>"])
  | "<enter_domain>" ->
    let terminal = "function enter_domain {\n\t enter(process,str2path(\"<<id>>/\"+$1));\n}".Replace("<<id>>", context.ProcessTreeName)
    ([terminal], [])
  | "<get_process>" ->
    (derivateGetProcess context, [])
  | "<newl>" ->
    (["\n"], [])
  | _ -> raise (ArgumentException("Unknown nonterminal - " + nonterminal))


let rec _createLanguage context (terminals, nonTerminals) =
  match nonTerminals |> List.length = 0 with
  | true ->
    (terminals, nonTerminals)
  | false ->
    let next = nonTerminals |> List.head
    let (newTerminals, newNonTerminals) = derivate context next
    _createLanguage context (newTerminals @ terminals, newNonTerminals @ List.skip 1 nonTerminals)


let createLanguage context =
  let (terminals, _) = _createLanguage context ([], ["<start>"])
  String.Join ("",terminals |> List.rev)
