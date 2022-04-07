module VirtualSpaceTests

    open NUnit.Framework
    open Shouldly
    open VirtualSpace
    open VirtualSpace.Types
    open CommonTypes

    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let ``PathEntry has correct isRecursive bool`` () =
        let input = [("/var/lib/mysql", Nametype.Parent)]
        let (path, nametype) = List.head input

        let pathEntry = { PathName = path; IsRecursive = isRecursive nametype; IsAddition = true; }
        pathEntry.IsRecursive.ShouldBeTrue ()
