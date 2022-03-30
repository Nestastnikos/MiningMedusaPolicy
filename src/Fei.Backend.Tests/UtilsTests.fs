module Fei.Backend.UtilTests

open NUnit.Framework
open Shouldly
open Utils

[<SetUp>]
let Setup () =
    ()

[<Test>]
let MapUtilsConcatCreatesNewList () =
    let input = Seq.ofList [Map ([| ("a", "b") |])]
    let result = MapUtils.concat input

    (Map.find "a" result).ShouldBe ["b"]

[<Test>]
let MapUtilsConcatJoinsOnlyElementsWithTheSameKey () =
    let input = Seq.ofList [Map ([| ("a", "b") |]); Map ([| ("b", "b")|])]
    let result = MapUtils.concat input

    (Map.find "a" result).ShouldBe ["b"]
    (Map.find "b" result).ShouldBe ["b"]

[<Test>]
let MapUtilsConcatJoinsValuesWithTheSameKeyToList () =
    let input = Seq.ofList [Map ([| ("a", "b") |]); Map ([| ("a", "c")|])]
    let result = MapUtils.concat input

    (Map.find "a" result).ShouldBe ["b"; "c"]

[<Test>]
let ListUtilsFstValueOrEmptyReturnsEmpty () =
    let result = ListUtils.fstValueOrNone []
    result.ShouldBe None

[<Test>]
let ListUtilsFstValueOrEmptyReturnsFirst () =
    let result = ListUtils.fstValueOrNone (["a"])
    result.ShouldBe (Some("a"))

[<Test>]
let ListUtilsFstValueOrEmptyReturnsFirstWhenMultipleValues () =
    let result = ListUtils.fstValueOrNone (["a"; "b"])
    result.ShouldBe (Some("a"))