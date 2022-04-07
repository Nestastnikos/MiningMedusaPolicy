module Fei.Backend.UtilTests

open NUnit.Framework
open Shouldly
open Utils

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``MapUtils Concat creates new list`` () =
    let input = Seq.ofList [Map ([| ("a", "b") |])]
    let result = MapUtils.concat input

    (Map.find "a" result).ShouldBe ["b"]

[<Test>]
let ``MapUtils Concat joins elements with the same key`` () =
    let input = Seq.ofList [Map ([| ("a", "b") |]); Map ([| ("b", "b")|])]
    let result = MapUtils.concat input

    (Map.find "a" result).ShouldBe ["b"]
    (Map.find "b" result).ShouldBe ["b"]

[<Test>]
let ``MapUtils Concat joins keys values with the same key to list`` () =
    let input = Seq.ofList [Map ([| ("a", "b") |]); Map ([| ("a", "c")|])]
    let result = MapUtils.concat input

    (Map.find "a" result).ShouldBe ["b"; "c"]

[<Test>]
let ``ListUtils - FstValueOrEmpty returns None when the list is empty``() =
    let result = ListUtils.fstValueOrNone []
    result.ShouldBe None

[<Test>]
let ``ListUtils - FstValueOrEmpty returns first when list contains only one value`` () =
    let result = ListUtils.fstValueOrNone (["a"])
    result.ShouldBe (Some("a"))

[<Test>]
let ``ListUtils - FstValueOrEmpty returns first when list contains multiple values`` () =
    let result = ListUtils.fstValueOrNone (["a"; "b"])
    result.ShouldBe (Some("a"))