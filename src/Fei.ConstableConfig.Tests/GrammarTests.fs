module GrammarTests

open NUnit.Framework
open DomainTypes
open Shouldly

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``createLanguage - derivate <file_tree> `` () =
    let context = GrammarContext.createDefault []
    let (terminals, nonTerminals) = GrammarContext._createLanguage context ([], ["<file_tree>"])
    (terminals |> List.length).ShouldBe 3
    (List.item 0 terminals).ShouldBe "primary tree \"fs\";"
    (List.item 1 terminals).ShouldBe "\n"
    (List.item 2 terminals).ShouldBe "tree \"fs\" clone of file by getfile getfile.filename;"

    (List.length nonTerminals).ShouldBe 0

[<Test>]
let ``createLanguage - derivate <process_tree> `` () =
    let context = GrammarContext.createDefault []
    let (terminals, nonTerminals) = GrammarContext._createLanguage context ([], ["<process_tree>"])
    (List.length nonTerminals).ShouldBe 0

    (terminals |> List.length).ShouldBe 1
    (List.item 0 terminals).ShouldBe "tree \"domains\" of process;"

[<Test>]
let ``createLanguage - derivate single <vs_proc_defs>`` () =
    let rule = { Subject = ("mysql", "/path/to/exec"); Object = ("", []); Permissions = [Read; Write] }
    let context = GrammarContext.createDefault [rule; rule]
    let (terminals, nonTerminals) = GrammarContext._createLanguage context ([], ["<vs_proc_defs>"])
    (List.length nonTerminals).ShouldBe 0

    (List.length terminals).ShouldBe 3
    (List.item 2 terminals).ShouldBe "mysql\tENTER mysql"
    (List.item 1 terminals).ShouldBe ";"
    (List.item 0 terminals).ShouldBe "\n"

[<Test>]
let ``createLanguage - derivate single <vs_fs_defs> `` () =
    let pathEntry = File({ FullPath = "/var/lib"; IsAddition = true })
    let rule = { Subject = ("", ""); Object = ("var_lib_rw", [pathEntry]); Permissions = [Read; Write] }
    let context = GrammarContext.createDefault [rule]
    let (terminals, nonTerminals) = GrammarContext._createLanguage context ([], ["<vs_fs_defs>"])
    (List.length nonTerminals).ShouldBe 0
    (List.length terminals).ShouldBe 3
    (List.item 2 terminals).ShouldBe "space var_lib_rw = /var/lib"
    (List.item 1 terminals).ShouldBe ";"
    (List.item 0 terminals).ShouldBe "\n"

[<Test>]
let ``createLanguage - derivate <vs_fs_defs> with multiple paths`` () =
    let pathEntries = [ File({ FullPath = "/var/lib"; IsAddition = true });
        Directory({ FullPath = "/var/log"; IsAddition = true; IsSticky = false; IsRecursive = true; }) ]
    let rule = { Subject = ("", ""); Object = ("var_with_children_rw", pathEntries); Permissions = [Read; Write] }
    let context = GrammarContext.createDefault [rule]
    let (terminals, nonTerminals) = GrammarContext._createLanguage context ([], ["<vs_fs_defs>"])
    (List.length nonTerminals).ShouldBe 0
    (List.length terminals).ShouldBe 5
    (List.item 4 terminals).ShouldBe "space var_with_children_rw = /var/lib"
    (List.item 3 terminals).ShouldBe "\n\t\t"
    (List.item 2 terminals).ShouldBe "+ recursive /var/log"
    (List.item 1 terminals).ShouldBe ";"
    (List.item 0 terminals).ShouldBe "\n"

[<Test>]
let ``createLanguage - derivate <rules> when single rule`` () =
    let rule = { Subject = ("mysql", "/path/to/exec"); Object = ("var_rw", []); Permissions = [Read; Write; See] }
    let context = GrammarContext.createDefault [rule]
    let (terminals, nonTerminals) = GrammarContext._createLanguage context ([], ["<rules>"])
    (List.length nonTerminals).ShouldBe 0
    (List.length terminals).ShouldBe 5
    (List.item 4 terminals).ShouldBe "mysql"
    (List.item 3 terminals).ShouldBe "\t"
    (List.item 2 terminals).ShouldBe "READ var_rw, WRITE var_rw, SEE var_rw"
    (List.item 1 terminals).ShouldBe ";"
    (List.item 0 terminals).ShouldBe "\n"

[<Test>]
let ``createLanguage - derivate <functions>`` () =
    let context = GrammarContext.createDefault []
    let (terminals, nonTerminals) = GrammarContext._createLanguage context ([], ["<functions>"])
    (List.length nonTerminals).ShouldBe 0
    (List.length terminals).ShouldBe 10
    (List.item 9 terminals).ShouldBe "function init"
    (List.item 8 terminals).ShouldBe "\n"
    (List.item 7 terminals).ShouldBe "{"
    (List.item 6 terminals).ShouldBe "}"
    (List.item 5 terminals).ShouldBe "\n"
    (List.item 4 terminals).ShouldBe "function constable_init"
    (List.item 3 terminals).ShouldBe "\n"
    (List.item 2 terminals).ShouldBe "{"
    (List.item 1 terminals).ShouldBe "}"
    (List.item 0 terminals).ShouldBe "\n"
