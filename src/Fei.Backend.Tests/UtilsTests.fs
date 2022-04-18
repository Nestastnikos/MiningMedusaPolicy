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
    let result = Some([]) |> ListUtils.fstValueOrNone
    result.ShouldBe None

[<Test>]
let ``ListUtils - FstValueOrEmpty returns first when list contains only one value`` () =
    let result = Some(["a"]) |> ListUtils.fstValueOrNone
    result.ShouldBe (Some("a"))

[<Test>]
let ``ListUtils - FstValueOrEmpty returns first when list contains multiple values`` () =
    let result = Some(["a"; "b"]) |> ListUtils.fstValueOrNone
    result.ShouldBe (Some("a"))

[<Test>]
let ``PathUtils - toPath - root dir to path`` () =
    let input = "/"
    let result = PathUtils.toPath input

    result.IsSome.ShouldBeTrue ()
    result.Value.FullPath.ShouldBe input
    result.Value.Segments.ShouldBe ["/"]
    result.Value.Depth.ShouldBe 0

[<Test>]
let ``PathUtils - toPath - nested dir to path`` () =
    let input = "/var"
    let result = PathUtils.toPath input
    result.IsSome.ShouldBeTrue ()
    result.Value.FullPath.ShouldBe input
    result.Value.Depth.ShouldBe 1
    result.Value.Segments.ShouldBe ["/"; "var"]

[<Test>]
let ``PathUtils - toPath - nested dir to path preserves ordering`` () =
    let input = "/var/lib"
    let result = PathUtils.toPath input
    result.IsSome.ShouldBeTrue ()
    result.Value.FullPath.ShouldBe input
    result.Value.Depth.ShouldBe 2
    result.Value.Segments.ShouldBe ["/"; "var"; "lib"]

[<Test>]
let ``PathUtils - toPath - file with extension is preserved as one`` () =
    let input = "/var/lib/mariadb.log"
    let result = PathUtils.toPath input
    result.IsSome.ShouldBeTrue ()
    result.Value.FullPath.ShouldBe input
    result.Value.Depth.ShouldBe 3
    result.Value.Segments.ShouldBe ["/"; "var"; "lib"; "mariadb.log"]

[<Test>]
let ``PathUtils - toPath - returns empty on path in non-canonic form`` () =
    let input = "halabala"
    let result = PathUtils.toPath input
    result.IsNone.ShouldBeTrue ()

[<Test>]
let ``PathUtils - getParent - returns parent`` () =
    let input = "/var/lib"
    let path = PathUtils.toPath input
    let parent = path |> CastUtils.optionToValueOrError |> PathUtils.getParentPath |> CastUtils.optionToValueOrError
    parent.FullPath.ShouldBe "/var"

[<Test>]
let ``StringUtils - skipLast skips the last character`` () =
    let input = "/test/"
    let result = input |> StringUtils.skipLast 1
    result.ShouldBe "/test"