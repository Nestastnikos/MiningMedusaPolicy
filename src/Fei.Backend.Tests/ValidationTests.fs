module ValidationTests

open NUnit.Framework
open Shouldly
open System
open Types.CommonTypes

[<TestCase ("123", 123)>]
[<TestCase ("0", 0)>]
[<TestCase ("01", 1)>]
let ``createId - can be a number`` (input, expected) =
  let result = Some(input) |> Validation.createId
  result.ShouldBe expected

[<TestCase ("-1")>]
[<TestCase ("-0")>]
[<TestCase ("ABCD")>]
let ``createId - throws if not a positive number`` input =
  Assert.Throws<ArgumentException> (fun () -> Validation.createId (Some(input)) |> ignore) |> ignore

[<Test>]
let ``createId - throws when argument is None`` () =
  Assert.Throws<ArgumentException> (fun () -> Validation.createId None |> ignore) |> ignore

[<TestCase ("/path/to/some/process")>]
[<TestCase ("/path/to/some/process_with_underscores")>]
[<TestCase ("/path/to/some/processContainingDigits123")>]
[<TestCase ("/PATH/TO/SOME/PROCESS_WITH_UPPERCASE")>]
[<TestCase ("/path/to/some/process-with-dashes")>]
[<TestCase ("/path/to/some/with-special-chars!:#.,$")>]
let ``createProctitle - can be a path`` input =
  let result = Some(input) |> Validation.createProctitle
  result.ShouldBe (input |> Proctitle)

[<Test>]
let ``createProctitle - throws on none`` () =
  Assert.Throws<ArgumentException> (fun () -> Validation.createProctitle None |> ignore) |> ignore

[<Test>]
let ``createProctitle - when path ends with /`` () =
  let input = "/path/to/some/process/"
  Assert.Throws<ArgumentException> (fun () -> Some(input) |> Validation.createProctitle |> ignore) |> ignore

[<Test>]
let ``createItems - returns empty when given no items`` () =
  let input = (Some([]), Some([]), Some([]))
  let result = input |||> Validation.createItems (Some("/"))
  (result |> List.length).ShouldBe 0

[<Test>]
let ``createItems - throws when some of the list is missing`` () =
  Assert.Throws<ArgumentException> (fun () -> (None, Some([]), Some([])) |||> Validation.createItems (Some("/")) |> ignore) |> ignore
  Assert.Throws<ArgumentException> (fun () -> (Some([]), None, Some([])) |||> Validation.createItems (Some("/")) |> ignore) |> ignore
  Assert.Throws<ArgumentException> (fun () -> (Some([]), Some([]), None) |||> Validation.createItems (Some("/")) |> ignore) |> ignore

[<Test>]
let ``createItems - throws the lists have different lengths`` () =
  Assert.Throws<ArgumentException> (fun () -> (Some(["0"]), Some([]), Some([])) |||> Validation.createItems (Some("/")) |> ignore) |> ignore
  Assert.Throws<ArgumentException> (fun () -> (Some(["0"]), Some([]), Some(["a"])) |||> Validation.createItems (Some("/")) |> ignore) |> ignore
  Assert.Throws<ArgumentException> (fun () -> (Some([]), Some(["/"]), Some(["a"])) |||> Validation.createItems (Some("/")) |> ignore) |> ignore

[<TestCase ("./test")>]
[<TestCase ("test")>]
let ``createItems - throws when cwd is not an absolute path`` input =
  Assert.Throws<ArgumentException> (fun () -> (Some([]), Some([]), Some([])) |||> Validation.createItems (Some(input)) |> ignore) |> ignore

[<TestCase ("/test//")>]
[<TestCase ("//")>]
let ``createItems - throws on incorrect cwd path`` input =
  Assert.Throws<ArgumentException> (fun () -> (Some([]), Some([]), Some([])) |||> Validation.createItems (Some(input)) |> ignore) |> ignore

[<TestCase ("./test", "/test")>]
[<TestCase ("/test/", "/test")>]
[<TestCase ("test", "/test")>]
[<TestCase ("/test", "/test")>]
let ``createItems - returns results on correct name path`` path =
  let (inputPath, expected) = path
  let input = (Some(["0"]), Some([inputPath]), Some(["PARENT"]))
  let result = input |||> Validation.createItems (Some("/"))
  let (path, nametype) = result |> List.head

  (result |> List.length).ShouldBe 1
  path.ShouldBe expected
  nametype.ShouldBe Parent

[<TestCase ("/test", "/var/lib/mysql", "/test")>]
[<TestCase ("./test", "/var/lib/mysql", "/var/lib/mysql/test")>]
[<TestCase ("plugin.MAI", "/var/lib/mysql", "/var/lib/mysql/plugin.MAI")>]
let ``createItems - can correctly concatenate cwd and item name`` path =
  let (inputPath, cwd, expected) = path
  let input = (Some(["0"]), Some([inputPath]), Some(["PARENT"]))
  let result = input |||> Validation.createItems (Some(cwd))
  let (path, nametype) = result |> List.head

  (result |> List.length).ShouldBe 1
  path.ShouldBe expected
  nametype.ShouldBe Parent

[<Test>]
let ``createModeAndStickyBit - creates correctly mode and sets sticky bit`` () =
  let input = "dir,sticky,777"
  let (mode, isSticky) = Some(input) |> Validation.createModeTypeAndStickyBit
  mode.ShouldBe (Validation.ModeType.Directory)
  isSticky.IsSome.ShouldBeTrue()
  isSticky.Value.ShouldBeTrue()

[<TestCase>]
let ``createModeAndStickyBit - creates mode without sticky bit`` () =
  let input = "dir,777"
  let (mode, isSticky) = Some(input) |> Validation.createModeTypeAndStickyBit
  mode.ShouldBe (Validation.ModeType.Directory)
  isSticky.IsSome.ShouldBeTrue()
  isSticky.Value.ShouldBeFalse()

[<Test>]
let ``createModeAndStickyBit creates mode as file`` () =
  let input = "file,777"
  let (mode, isSticky) = Some(input) |> Validation.createModeTypeAndStickyBit
  mode.ShouldBe (Validation.ModeType.File)
  isSticky.IsNone.ShouldBeTrue()

[<Test>]
let ``createModeAndStickyBit throws when sticky is combined with anything except directory`` () =
  let input = "file,sticky,777"
  Assert.Throws<ArgumentException> (fun () -> Some(input) |> Validation.createModeTypeAndStickyBit |> ignore) |> ignore

[<Test>]
let ``createModeAndStickyBit throws on None input`` () =
  Assert.Throws<ArgumentException> (fun () -> None |> Validation.createModeTypeAndStickyBit |> ignore) |> ignore

[<Test>]
let ``createUid creates Uid`` () =
  let input = "mysql"
  let result = Some(input) |> Validation.createUid
  result.ShouldBe (input |> Uid)

[<Test>]
let ``createUid throws on None`` () =
  Assert.Throws<ArgumentException> (fun () -> None |> Validation.createUid |> ignore) |> ignore