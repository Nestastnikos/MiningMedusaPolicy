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
                Paths = fsVs1.Paths @ fsVs2.Paths }
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
            Paths = fsVs1.Paths @ fsVs2.Paths;
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
            Paths = fsVs1.Paths @ fsVs2.Paths @ fsVs3.Paths;
        }

        let expectedRule = {
            Subject = processIdentifier; Object = expectedFsVs; Permissions = AllVsPermissions
        }

        let result = PolicyMining.mergeRules input
        result.Length.ShouldBe 1
        (List.head result).ShouldBe expectedRule

    [<Test>]
    let ``getNameForVirtualSpace - rule with / is renamed to all_files`` () =
        let path = CastUtils.optionToValueOrError (PathUtils.toPath "/")
        let input = { Identifier = "/"; Paths = [{ Path = path; IsRecursive = false; IsAddition = true }]}
        let result = PolicyMining.getNameForVirtualSpace input.Paths
        result.ShouldBe "all_files"

    [<Test>]
    let ``getNameForVirtualSpace - rule with one path has replaced / with _`` () =
        let path = CastUtils.optionToValueOrError (PathUtils.toPath "/var/lib/mysql")
        let input = { Identifier = "/"; Paths = [{ Path = path; IsRecursive = false; IsAddition = true }]}
        let result = PolicyMining.getNameForVirtualSpace input.Paths
        result.ShouldBe "var_lib_mysql"

    [<Test>]
    let ``getNameForVirtualSpace - rule with multiple paths on the same depth have 'some' suffix`` () =
        let fullPath1 = "/var/lib/mysql/mariadb.log"
        let fullPath2 = "/var/lib/mysql/passwd.log"
        let path1 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath1)
        let path2 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath2)

        let fsVs = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = false; IsAddition = true };
                        { Path = path2; IsRecursive = false; IsAddition = true }]}
        let result = PolicyMining.getNameForVirtualSpace fsVs.Paths
        result.ShouldBe "var_lib_mysql_some"

    [<Test>]
    let ``getNameForVirtualSpace - rule with restrictive path on the same depth have 'some' suffix`` () =
        let fullPath1 = "/var/lib/mysql/mariadb.log"
        let fullPath2 = "/var/lib/mysql/passwd.log"
        let path1 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath1)
        let path2 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath2)

        let fsVs = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = false; IsAddition = true };
                        { Path = path2; IsRecursive = false; IsAddition = false}]}
        let result = PolicyMining.getNameForVirtualSpace fsVs.Paths
        result.ShouldBe "var_lib_mysql_some"

    [<Test>]
    let ``getNameForVirtualSpace - rule with multiple paths on different depth have 'with_children' suffix`` () =
        let fullPath1 = "/var/lib/mysql"
        let fullPath2 = "/var/lib/mysql/passwd.log"
        let path1 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath1)
        let path2 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath2)

        let fsVs = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true };
                        { Path = path2; IsRecursive = false; IsAddition = true }]}
        let result = PolicyMining.getNameForVirtualSpace fsVs.Paths
        result.ShouldBe "var_lib_mysql_with_children"

    [<Test>]
    let ``getNameForVirtualSpace - with recursive parent and restrictive path on different depth have 'with_exceptions' suffix`` () =
        let fullPath1 = "/var/lib/mysql"
        let fullPath2 = "/var/lib/mysql/passwd.log"
        let path1 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath1)
        let path2 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath2)

        let fsVs = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true };
                        { Path = path2; IsRecursive = false; IsAddition = false}]}
        let result = PolicyMining.getNameForVirtualSpace fsVs.Paths
        result.ShouldBe "var_lib_mysql_with_exceptions"

    [<Test>]
    let ``getNameForVirtualSpace recursive parent with both restrictive and additive children have 'with_children_with_exceptions' suffix`` () =
        let fullPath1 = "/var/lib/mysql"
        let fullPath2 = "/var/lib/mysql/passwd.log"
        let fullPath3 = "/var/lib/mysql/secrets.txt"
        let path1 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath1)
        let path2 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath2)
        let path3 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath3)

        let fsVs = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true };
                        { Path = path2; IsRecursive = false; IsAddition = false};
                        { Path = path3; IsRecursive = false; IsAddition = true }]}
        let result = PolicyMining.getNameForVirtualSpace fsVs.Paths
        result.ShouldBe "var_lib_mysql_with_children_with_exceptions"

    [<Test>]
    let ``simplifyRule - leaves already simplified rule as it is`` () =
        let fullPath1 = "/var/lib/mysql"
        let path1 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath1)

        let fsVs = { Identifier = "var_lib_mysql"; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true }] }
        let result = PolicyMining.simplifyVirtualSpace fsVs
        result.ShouldBe fsVs

    [<Test>]
    let ``simplifyRule - simplified vs with two paths to one when contains recursive directory`` () =
        let fullPath1 = "/var/lib/mysql"
        let fullPath2 = "/var/lib/mysql/mariadb.log"
        let path1 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath1)
        let path2 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath2)

        let fsVs = { Identifier = "var_lib_mysql_with_children"; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true };
                    { Path = path2; IsRecursive = true; IsAddition = true }] }

        let expectedFsVs = { Identifier = "var_lib_mysql"; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true }; ]}

        let result = PolicyMining.simplifyVirtualSpace fsVs
        result.ShouldBe expectedFsVs

    [<Test>]
    let ``simplifyRule - rule with two rules where one is addition and another not won't change anything`` () =
        let fullPath1 = "/var/lib/mysql"
        let fullPath2 = "/var/lib/mysql/mariadb.log"
        let path1 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath1)
        let path2 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath2)

        let fsVs = { Identifier = "var_lib_mysql_with_exceptions"; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true };
                    { Path = path2; IsRecursive = false; IsAddition = false }] }

        let result = PolicyMining.simplifyVirtualSpace fsVs
        result.ShouldBe fsVs

    [<Test>]
    let ``simplifyRule - rule with multiple recursive dirs will leave only top recursive dir`` () =
        let fullPath1 = "/var/lib/mysql"
        let fullPath2 = "/var/lib/mysql/mysql"
        let fullPath3 = "/var/lib/mysql/moetechta"
        let path1 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath1)
        let path2 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath2)
        let path3 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath3)

        let fsVs = { Identifier = "var_lib_mysql_with_children"; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true };
                    { Path = path2; IsRecursive = true; IsAddition = true };
                    { Path = path3; IsRecursive = true; IsAddition = true }] }

        let expectedFsVs = { Identifier = "var_lib_mysql"; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true }]}

        let result = PolicyMining.simplifyVirtualSpace fsVs
        result.Paths.Length.ShouldBe 1
        result.ShouldBe expectedFsVs

    [<Test>]
    let ``simplifyRule - rule with multiple recursive dirs on different depths sharing different parents`` () =
        let fullPath1 = "/var/lib"
        let fullPath2 = "/var/log/mysql"
        let fullPath3 = "/var/etc/nosql/moetechta"
        let path1 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath1)
        let path2 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath2)
        let path3 = CastUtils.optionToValueOrError (PathUtils.toPath fullPath3)

        let fsVs = { Identifier = "var_with_children"; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true };
                    { Path = path2; IsRecursive = true; IsAddition = true };
                    { Path = path3; IsRecursive = true; IsAddition = true }] }

        let result = PolicyMining.simplifyVirtualSpace fsVs
        result.Paths.Length.ShouldBe 3
        result.ShouldBe fsVs