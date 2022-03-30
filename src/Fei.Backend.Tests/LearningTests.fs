module Fei.Backend.LearningTests

open NUnit.Framework
open Shouldly
open System.Text.RegularExpressions

open Utils

[<SetUp>]
let Setup () =
    ()

[<Test>]
let RegexMatchesSimpleEntry () =
    let inputLine = "type=PROCTITLE : proctitle=/usr/libexec/mariadbd --basedir=/usr"

    let pattern = "[a-z_0-9]+=[a-zA-Z0-9_:\\/]+"
    let result = Regex.Matches (inputLine, pattern)
    result.Count.ShouldBe 3
    result.[0].Value.ShouldBe "type=PROCTITLE"
    result.[1].Value.ShouldBe "proctitle=/usr/libexec/mariadbd"
    result.[2].Value.ShouldBe "basedir=/usr"

[<Test>]
let RegexMatchesContentInsideParentheses () =
    let inputLine = "type=CWD msg=audit(03/10/22 18:48:20.282:283) : cwd=/"
    let pattern = "[a-z_]+=[a-zA-Z]*\\([a-zA-Z0-9\\/\\s:.]+\\)"
    let result = Regex.Matches (inputLine, pattern)
    result.Count.ShouldBe 1
    result.[0].Value.ShouldBe "msg=audit(03/10/22 18:48:20.282:283)"

[<Test>]
let RegexCombined () =
    let inputLine = "type=PROCTITLE msg=audit(03/10/22 18:48:20.282:283) : proctitle=/usr/libexec/mariadbd --basedir=/usr"
    let pattern = "\\w+=[\\w:\\/]+(\\([\\w\\/\\s:.]+\\)){0,1}"
    let result = Regex.Matches (inputLine, pattern)
    result.Count.ShouldBe 4
    result.[0].Value.ShouldBe "type=PROCTITLE"
    result.[1].Value.ShouldBe "msg=audit(03/10/22 18:48:20.282:283)"
    result.[2].Value.ShouldBe "proctitle=/usr/libexec/mariadbd"
    result.[3].Value.ShouldBe "basedir=/usr"

[<Test>]
let ParseIdentifier () =
    let input = "msg=audit(03/10/22 18:48:20.282:283)"
    let pattern = "(?<=:)[0-9]+(?=\\))"

    let result = Regex.Match (input, pattern)
    result.Value.ShouldBe "283"

[<Test>]
let StringGetCanonicalPath () =
    let example1 = "/test"
    let example2 = "./test"
    let cwd = "/"

    (PathUtils.getCanonicalPath cwd example1).ShouldBe "/test"
    (PathUtils.getCanonicalPath cwd example2).ShouldBe "/test"

[<Test>]
let ListOrdering () =
    let input = [(1, "Item2") ; (0, "Item1")]
    let output = input |> List.sortBy (fun (x,y) -> x)
    (List.head output).ShouldBe ((0, "Item1"))
