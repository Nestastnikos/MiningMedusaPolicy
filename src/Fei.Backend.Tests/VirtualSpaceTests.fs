module VirtualSpaceTests

    open NUnit.Framework
    open Shouldly
    open Types.CommonTypes
    open VirtualSpace
    open VirtualSpace.Types
    open VirtualSpace.Constants
    open Utils

    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let ``PathEntry has correct isRecursive bool`` () =
        let input = [("/var/lib/mysql", Nametype.Parent)]
        let (fullPath, nametype) = List.head input
        let path = CastUtils.optionToValueOrError (PathUtils.toPath fullPath)

        let pathEntry = { Path = path; IsRecursive = isRecursive nametype; IsAddition = true; }
        pathEntry.IsRecursive.ShouldBeTrue ()

    [<Test>]
    let ``mergeRules leave single rule as it is`` () =
        let processIdentifier = ("mysql", "/var/lib/mariadbd");

        let fullPath = "/var/lib/mysql"
        let path = CastUtils.optionToValueOrError (PathUtils.toPath fullPath)
        let fsVs = { Identifier = fullPath; Paths = [{ Path = path; IsRecursive = false; IsAddition = true }]}
        let input = [ { Subject = processIdentifier; Object = fsVs; Permissions = AllVsPermissions }]

        let result = PolicyMining.mergeRules input
        let rule = List.head result
        rule.Subject.ShouldBe processIdentifier
        rule.Object.ShouldBe fsVs
        rule.Permissions.ShouldBe AllVsPermissions

    [<Test>]
    let ``mergeRules wont merge rules on depth 1`` () =
        let processIdentifier = ("mysql", "/var/lib/mariadbd");

        let fullPath1 = "/etc"
        let fullPath2 = "/var"
        let path1 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath1)
        let path2 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath2)

        let fsVs1 = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true }] }
        let fsVs2 = { Identifier = fullPath2; Paths = [{ Path = path2; IsRecursive = true; IsAddition = true }] }
        let input = [ { Subject = processIdentifier; Object = fsVs1; Permissions = AllVsPermissions };
                        { Subject = processIdentifier; Object = fsVs2; Permissions = AllVsPermissions }; ]

        let result = PolicyMining.mergeRules input
        result.ShouldBe input

    [<Test>]
    let ``mergeRules merge two rules whose fs virtual space contain single file path`` () =
        let processIdentifier = ("mysql", "/var/lib/mariadbd");

        let fullPath1 = "/var/lib/mysql/mysql.log"
        let fullPath2 = "/var/lib/mysql/MAIN.EDI"
        let path1 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath1)
        let path2 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath2)

        let fsVs1 = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = false; IsAddition = true }]}
        let fsVs2 = { Identifier = fullPath2; Paths = [{ Path = path2; IsRecursive = false; IsAddition = true }]}
        let input = [ { Subject = processIdentifier; Object = fsVs1; Permissions = AllVsPermissions };
                        { Subject = processIdentifier; Object = fsVs2; Permissions = AllVsPermissions }]

        let expectedRule = {
            Subject = processIdentifier;
            Object = {
                Identifier = "var_lib_mysql_some";
                Paths = [{ Path = path1; IsRecursive = false; IsAddition = true};
                            { Path = path2; IsRecursive = false; IsAddition = true; }]};
            Permissions = AllVsPermissions
            }

        let result = PolicyMining.mergeRules input
        result.Length.ShouldBe 1

        let rule = List.head result
        rule.ShouldBe expectedRule

    [<Test>]
    let ``mergeRules merge two rules where rule contain fs vs which is parent of fs vs in second rule`` () =
        let processIdentifier = ("mysql", "/var/lib/mariadbd");

        let fullPath1 = "/var/lib/mysql"
        let fullPath2 = "/var/lib/mysql/MAIN.EDI"
        let path1 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath1)
        let path2 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath2)

        let fsVs1 = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true }]}
        let fsVs2 = { Identifier = fullPath2; Paths = [{ Path = path2; IsRecursive = false; IsAddition = true }]}
        let input = [ { Subject = processIdentifier; Object = fsVs1; Permissions = AllVsPermissions };
                        { Subject = processIdentifier; Object = fsVs2; Permissions = AllVsPermissions }]

        let expectedRule = {
            Subject = processIdentifier;
            Object = {
                Identifier = "var_lib_mysql_with_children";
                Paths = [{ Path = path1; IsRecursive = false; IsAddition = true};
                            { Path = path2; IsRecursive = false; IsAddition = true; }]};
            Permissions = AllVsPermissions
            }

        let result = PolicyMining.mergeRules input
        result.Length.ShouldBe 1

        let rule = List.head result
        rule.ShouldBe expectedRule

    [<Test>]
    let ``mergeRules won't merge two rules where rules contain fs vs's which do not share the same parent`` () =
        let processIdentifier = ("mysql", "/var/lib/mariadbd");

        let fullPath1 = "/etc/lib/mysql"
        let fullPath2 = "/var/lib/mysql/MAIN.EDI"
        let path1 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath1)
        let path2 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath2)

        let fsVs1 = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true }]}
        let fsVs2 = { Identifier = fullPath2; Paths = [{ Path = path2; IsRecursive = false; IsAddition = true }]}
        let input = [ { Subject = processIdentifier; Object = fsVs1; Permissions = AllVsPermissions };
                        { Subject = processIdentifier; Object = fsVs2; Permissions = AllVsPermissions }]

        let result = PolicyMining.mergeRules input
        result.Length.ShouldBe 2
        result.ShouldBe input

    [<Test>]
    let ``mergeRules will remove duplicate rules`` () =
        let processIdentifier = ("mysql", "/var/lib/mariadbd");

        let fullPath = "/etc/lib/mysql"
        let path = CastUtils.optionToValueOrError (PathUtils.toPath fullPath)

        let fsVs1 = { Identifier = fullPath; Paths = [{ Path = path; IsRecursive = false; IsAddition = true }]}
        let fsVs2 = { Identifier = fullPath; Paths = [{ Path = path; IsRecursive = false; IsAddition = true }]}
        let input = [ { Subject = processIdentifier; Object = fsVs1; Permissions = AllVsPermissions };
                        { Subject = processIdentifier; Object = fsVs2; Permissions = AllVsPermissions }; ]

        let result = PolicyMining.mergeRules input
        result.Length.ShouldBe 1
        (List.head result).ShouldBe (List.head input)

    [<Test>]
    let ``mergeRules will create rules with exceptions`` () =
        let processIdentifier = ("mysql", "/var/lib/mariadbd");

        let fullPath1 = "/etc/lib/mysql"
        let fullPath2 = "/etc/lib/mysql/mariadb.log"
        let path1 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath1)
        let path2 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath2)

        let fsVs1 = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true }]}
        let fsVs2 = { Identifier = fullPath2; Paths = [{ Path = path2; IsRecursive = false; IsAddition = false }]}
        let input = [ { Subject = processIdentifier; Object = fsVs1; Permissions = AllVsPermissions };
                        { Subject = processIdentifier; Object = fsVs2; Permissions = AllVsPermissions }; ]

        let expectedFsVs = {
            Identifier = "etc_lib_mysql_with_exceptions";
            Paths = [{ Path = path1; IsRecursive = true; IsAddition = true}; { Path = path2; IsRecursive = false; IsAddition = false }];
        }
        let expectedRule = {
            Subject = processIdentifier; Object = expectedFsVs; Permissions = AllVsPermissions
        }

        let result = PolicyMining.mergeRules input
        result.Length.ShouldBe 1
        (List.head result).ShouldBe expectedRule


    [<Test>]
    let ``mergeRules can merge both additions and exceptions to single rule`` () =
        let processIdentifier = ("mysql", "/var/lib/mariadbd");

        let fullPath1 = "/etc/lib/mysql"
        let fullPath2 = "/etc/lib/mysql/mariadb.log"
        let fullPath3 = "/etc/lib/mysql/passwd.log"
        let path1 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath1)
        let path2 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath2)
        let path3 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath3)

        let fsVs1 = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = false; IsAddition = true }]}
        let fsVs2 = { Identifier = fullPath2; Paths = [{ Path = path2; IsRecursive = false; IsAddition = true }]}
        let fsVs3 = { Identifier = fullPath3; Paths = [{ Path = path3; IsRecursive = false; IsAddition = false }]}
        let input = [ { Subject = processIdentifier; Object = fsVs1; Permissions = AllVsPermissions };
                        { Subject = processIdentifier; Object = fsVs2; Permissions = AllVsPermissions };
                        { Subject = processIdentifier; Object = fsVs3; Permissions = AllVsPermissions }; ]

        let expectedFsVs = {
            Identifier = "etc_lib_mysql_with_children_with_exceptions";
            Paths = [{ Path = path1; IsRecursive = false ; IsAddition = true};
                        { Path = path2; IsRecursive = false; IsAddition = true};
                        { Path = path3; IsRecursive = false; IsAddition = false }; ];
        }

        let expectedRule = {
            Subject = processIdentifier; Object = expectedFsVs; Permissions = AllVsPermissions
        }

        let result = PolicyMining.mergeRules input
        result.Length.ShouldBe 1
        (List.head result).ShouldBe expectedRule
