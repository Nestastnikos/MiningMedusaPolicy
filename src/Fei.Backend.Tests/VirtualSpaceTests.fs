module VirtualSpaceTests

open NUnit.Framework
open Shouldly
open Types.CommonTypes
open VirtualSpaceTypes
open VirtualSpaceTypes.Constants
open Utils

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``PathEntry has correct isRecursive bool`` () =
    let input = [("/var/lib/mysql", Nametype.Parent)]
    let (fullPath, nametype) = List.head input
    let path = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath)

    let pathEntry = { Path = path; IsRecursive = PolicyMining.isRecursive nametype; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }
    pathEntry.IsRecursive.ShouldBeTrue ()

[<Test>]
let ``mergeRules leave single rule as it is`` () =
    let processIdentifier = ("mysql" |> Uid, "/var/lib/mariadbd" |> Proctitle);

    let fullPath = "/var/lib/mysql"
    let path = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath)
    let fsVs = { Identifier = fullPath; Paths = [{ Path = path; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }]}
    let input = [ { Subject = processIdentifier; Object = fsVs; Permissions = AllVsPermissions }]

    let result = PolicyMining.mergeRules input
    let rule = List.head result
    rule.Subject.ShouldBe processIdentifier
    rule.Object.ShouldBe fsVs
    rule.Permissions.ShouldBe AllVsPermissions

[<Test>]
let ``mergeRules wont merge rules on depth 1`` () =
    let processIdentifier = ("mysql" |> Uid, "/var/lib/mariadbd" |> Proctitle);

    let fullPath1 = "/etc"
    let fullPath2 = "/var"
    let path1 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath1)
    let path2 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath2)

    let fsVs1 = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }] }
    let fsVs2 = { Identifier = fullPath2; Paths = [{ Path = path2; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }] }
    let input = [ { Subject = processIdentifier; Object = fsVs1; Permissions = AllVsPermissions };
                    { Subject = processIdentifier; Object = fsVs2; Permissions = AllVsPermissions }; ]

    let result = PolicyMining.mergeRules input
    result.ShouldBe input

[<Test>]
let ``mergeRules merge two rules whose fs virtual space contain single file path`` () =
    let processIdentifier = ("mysql" |> Uid, "/var/lib/mariadbd" |> Proctitle);

    let fullPath1 = "/var/lib/mysql/mysql.log"
    let fullPath2 = "/var/lib/mysql/MAIN.EDI"
    let path1 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath1)
    let path2 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath2)

    let fsVs1 = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = false; }]}
    let fsVs2 = { Identifier = fullPath2; Paths = [{ Path = path2; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = false; }]}
    let input = [ { Subject = processIdentifier; Object = fsVs1; Permissions = AllVsPermissions };
                    { Subject = processIdentifier; Object = fsVs2; Permissions = AllVsPermissions }]

    let expectedRule = {
        Subject = processIdentifier;
        Object = {
            Identifier = "var_lib_mysql_some_rws";
            Paths = [{ Path = path1; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = false;};
                        { Path = path2; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = false; }]};
        Permissions = AllVsPermissions
        }

    let result = PolicyMining.mergeRules input
    result.Length.ShouldBe 1

    let rule = List.head result
    rule.ShouldBe expectedRule

[<Test>]
let ``mergeRules merge two rules where rule contain fs vs which is parent of fs vs in second rule`` () =
    let processIdentifier = ("mysql" |> Uid, "/var/lib/mariadbd" |> Proctitle);

    let fullPath1 = "/var/lib/mysql"
    let fullPath2 = "/var/lib/mysql/MAIN.EDI"
    let path1 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath1)
    let path2 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath2)

    let fsVs1 = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }]}
    let fsVs2 = { Identifier = fullPath2; Paths = [{ Path = path2; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = false; }]}
    let input = [ { Subject = processIdentifier; Object = fsVs1; Permissions = AllVsPermissions };
                    { Subject = processIdentifier; Object = fsVs2; Permissions = AllVsPermissions }]

    let expectedRule = {
        Subject = processIdentifier;
        Object = {
            Identifier = "var_lib_mysql_with_children_rws";
            Paths = fsVs1.Paths @ fsVs2.Paths }
        Permissions = AllVsPermissions
        }

    let result = PolicyMining.mergeRules input
    result.Length.ShouldBe 1

    let rule = List.head result
    rule.ShouldBe expectedRule

[<Test>]
let ``mergeRules won't merge two rules where rules contain fs vs's which do not share the same parent`` () =
    let processIdentifier = ("mysql" |> Uid, "/var/lib/mariadbd" |> Proctitle);

    let fullPath1 = "/etc/lib/mysql"
    let fullPath2 = "/var/lib/mysql/MAIN.EDI"
    let path1 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath1)
    let path2 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath2)

    let fsVs1 = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }]}
    let fsVs2 = { Identifier = fullPath2; Paths = [{ Path = path2; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = false; }]}
    let input = [ { Subject = processIdentifier; Object = fsVs1; Permissions = AllVsPermissions };
                    { Subject = processIdentifier; Object = fsVs2; Permissions = AllVsPermissions }]

    let result = PolicyMining.mergeRules input
    result.Length.ShouldBe 2
    result.ShouldBe input

[<Test>]
let ``mergeRules will remove duplicate rules`` () =
    let processIdentifier = ("mysql" |> Uid, "/var/lib/mariadbd" |> Proctitle);

    let fullPath = "/etc/lib/mysql"
    let path = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath)

    let fsVs1 = { Identifier = fullPath; Paths = [{ Path = path; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }]}
    let fsVs2 = { Identifier = fullPath; Paths = [{ Path = path; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }]}
    let input = [ { Subject = processIdentifier; Object = fsVs1; Permissions = AllVsPermissions };
                    { Subject = processIdentifier; Object = fsVs2; Permissions = AllVsPermissions }; ]

    let result = PolicyMining.mergeRules input
    result.Length.ShouldBe 1
    (List.head result).ShouldBe (List.head input)

[<Test>]
let ``mergeRules will create rules with exceptions`` () =
    let processIdentifier = ("mysql" |> Uid, "/var/lib/mariadbd" |> Proctitle);

    let fullPath1 = "/etc/lib/mysql"
    let fullPath2 = "/etc/lib/mysql/mariadb.log"
    let path1 = PathUtils.toPath fullPath1
    let path2 = PathUtils.toPath fullPath2

    let fsVs1 = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }]}
    let fsVs2 = { Identifier = fullPath2; Paths = [{ Path = path2; IsRecursive = false; IsAddition = false; IsSticky = Some(false); IsDirectory = false; }]}
    let input = [ { Subject = processIdentifier; Object = fsVs1; Permissions = AllVsPermissions };
                    { Subject = processIdentifier; Object = fsVs2; Permissions = AllVsPermissions }; ]

    let expectedFsVs = {
        Identifier = "etc_lib_mysql_with_exceptions_rws";
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
    let processIdentifier = ("mysql" |> Uid, "/var/lib/mariadbd" |> Proctitle);

    let fullPath1 = "/etc/lib/mysql"
    let fullPath2 = "/etc/lib/mysql/mariadb.log"
    let fullPath3 = "/etc/lib/mysql/passwd.log"
    let path1 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath1)
    let path2 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath2)
    let path3 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath3)

    let fsVs1 = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }]}
    let fsVs2 = { Identifier = fullPath2; Paths = [{ Path = path2; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = false; }]}
    let fsVs3 = { Identifier = fullPath3; Paths = [{ Path = path3; IsRecursive = false; IsAddition = false; IsSticky = Some(false); IsDirectory = false; }]}
    let input = [ { Subject = processIdentifier; Object = fsVs1; Permissions = AllVsPermissions };
                    { Subject = processIdentifier; Object = fsVs2; Permissions = AllVsPermissions };
                    { Subject = processIdentifier; Object = fsVs3; Permissions = AllVsPermissions }; ]

    let expectedFsVs = {
        Identifier = "etc_lib_mysql_with_children_with_exceptions_rws";
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
    let path = CastUtils.optionToValueOrError (PathUtils.tryToPath "/")
    let input = { Identifier = "/"; Paths = [{ Path = path; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }]}
    let result = PolicyMining.getNameForVirtualSpace input.Paths AllVsPermissions
    result.ShouldBe "all_files_rws"

[<Test>]
let ``getNameForVirtualSpace - rule with one path has replaced / with _`` () =
    let path = CastUtils.optionToValueOrError (PathUtils.tryToPath "/var/lib/mysql")
    let input = { Identifier = "/"; Paths = [{ Path = path; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }]}
    let result = PolicyMining.getNameForVirtualSpace input.Paths AllVsPermissions
    result.ShouldBe "var_lib_mysql_rws"

[<Test>]
let ``getNameForVirtualSpace - rule with multiple paths on the same depth have 'some' suffix`` () =
    let fullPath1 = "/var/lib/mysql/mariadb.log"
    let fullPath2 = "/var/lib/mysql/passwd.log"
    let path1 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath1)
    let path2 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath2)

    let fsVs = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = false; };
                    { Path = path2; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = false; }]}
    let result = PolicyMining.getNameForVirtualSpace fsVs.Paths AllVsPermissions
    result.ShouldBe "var_lib_mysql_some_rws"

[<Test>]
let ``getNameForVirtualSpace - rule with restrictive path on the same depth have 'some' suffix`` () =
    let fullPath1 = "/var/lib/mysql/mariadb.log"
    let fullPath2 = "/var/lib/mysql/passwd.log"
    let path1 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath1)
    let path2 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath2)

    let fsVs = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = false; };
                    { Path = path2; IsRecursive = false; IsAddition = false; IsSticky = Some(false); IsDirectory = false;}]}
    let result = PolicyMining.getNameForVirtualSpace fsVs.Paths AllVsPermissions
    result.ShouldBe "var_lib_mysql_some_rws"

[<Test>]
let ``getNameForVirtualSpace - rule with multiple paths on different depth have 'with_children' suffix`` () =
    let fullPath1 = "/var/lib/mysql"
    let fullPath2 = "/var/lib/mysql/passwd.log"
    let path1 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath1)
    let path2 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath2)

    let fsVs = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; };
                    { Path = path2; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = false; }]}
    let result = PolicyMining.getNameForVirtualSpace fsVs.Paths AllVsPermissions
    result.ShouldBe "var_lib_mysql_with_children_rws"

[<Test>]
let ``getNameForVirtualSpace - with recursive parent and restrictive path on different depth have 'with_exceptions' suffix`` () =
    let fullPath1 = "/var/lib/mysql"
    let fullPath2 = "/var/lib/mysql/passwd.log"
    let path1 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath1)
    let path2 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath2)

    let fsVs = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; };
                    { Path = path2; IsRecursive = false; IsAddition = false; IsSticky = Some(false); IsDirectory = false;}]}
    let result = PolicyMining.getNameForVirtualSpace fsVs.Paths AllVsPermissions
    result.ShouldBe "var_lib_mysql_with_exceptions_rws"

[<Test>]
let ``getNameForVirtualSpace recursive parent with both restrictive and additive children have 'with_children_with_exceptions' suffix`` () =
    let fullPath1 = "/var/lib/mysql"
    let fullPath2 = "/var/lib/mysql/passwd.log"
    let fullPath3 = "/var/lib/mysql/secrets.txt"
    let path1 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath1)
    let path2 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath2)
    let path3 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath3)

    let fsVs = { Identifier = fullPath1; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; };
                    { Path = path2; IsRecursive = false; IsAddition = false; IsSticky = Some(false); IsDirectory = false;};
                    { Path = path3; IsRecursive = false; IsAddition = true; IsSticky = Some(false); IsDirectory = false; }]}
    let result = PolicyMining.getNameForVirtualSpace fsVs.Paths AllVsPermissions
    result.ShouldBe "var_lib_mysql_with_children_with_exceptions_rws"

[<Test>]
let ``simplifyRule - leaves already simplified rule as it is`` () =
    let fullPath1 = "/var/lib/mysql"
    let path1 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath1)

    let fsVs = { Identifier = "var_lib_mysql_rws"; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }] }
    let result = PolicyMining.simplifyVirtualSpace AllVsPermissions fsVs
    result.ShouldBe fsVs

[<Test>]
let ``simplifyRule - simplified vs with two paths to one when contains recursive directory`` () =
    let fullPath1 = "/var/lib/mysql"
    let fullPath2 = "/var/lib/mysql/mariadb.log"
    let path1 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath1)
    let path2 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath2)

    let fsVs = { Identifier = "var_lib_mysql_with_children"; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; };
                { Path = path2; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = false; }] }

    let expectedFsVs = { Identifier = "var_lib_mysql_rws"; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }; ]}

    let result = PolicyMining.simplifyVirtualSpace AllVsPermissions fsVs
    result.ShouldBe expectedFsVs

[<Test>]
let ``simplifyRule - rule with two rules where one is addition and another not won't change anything`` () =
    let fullPath1 = "/var/lib/mysql"
    let fullPath2 = "/var/lib/mysql/mariadb.log"
    let path1 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath1)
    let path2 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath2)

    let fsVs = { Identifier = "var_lib_mysql_with_exceptions_rws"; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; };
                { Path = path2; IsRecursive = false; IsAddition = false; IsSticky = Some(false); IsDirectory = false; }] }

    let result = PolicyMining.simplifyVirtualSpace AllVsPermissions fsVs
    result.ShouldBe fsVs

[<Test>]
let ``simplifyRule - rule with multiple recursive dirs will leave only top recursive dir`` () =
    let fullPath1 = "/var/lib/mysql"
    let fullPath2 = "/var/lib/mysql/mysql"
    let fullPath3 = "/var/lib/mysql/moetechta"
    let path1 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath1)
    let path2 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath2)
    let path3 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath3)

    let fsVs = { Identifier = "var_lib_mysql_with_children"; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; };
                { Path = path2; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; };
                { Path = path3; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }] }

    let expectedFsVs = { Identifier = "var_lib_mysql_rws"; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }]}

    let result = PolicyMining.simplifyVirtualSpace AllVsPermissions fsVs
    result.Paths.Length.ShouldBe 1
    result.ShouldBe expectedFsVs

[<Test>]
let ``simplifyRule - rule with multiple recursive dirs on different depths sharing different parents`` () =
    let fullPath1 = "/var/lib"
    let fullPath2 = "/var/log/mysql"
    let fullPath3 = "/var/etc/nosql/moetechta"
    let path1 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath1)
    let path2 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath2)
    let path3 = CastUtils.optionToValueOrError (PathUtils.tryToPath fullPath3)

    let fsVs = { Identifier = "var_with_children_rws"; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; };
                { Path = path2; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; };
                { Path = path3; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }] }

    let result = PolicyMining.simplifyVirtualSpace AllVsPermissions fsVs
    result.Paths.Length.ShouldBe 3
    result.ShouldBe fsVs

[<Test>]
let ``mergeRule - rules with different permissions are not merged`` () =
    let subject = ("mysql", "/lib/mariadb")
    let fullPath1 = "/var/lib"
    let fullPath2 = "/var/log/mysql"
    let path1 = PathUtils.toPath fullPath1
    let path2 = PathUtils.toPath fullPath2

    let fsVs1 = { Identifier = "var_lib_r"; Paths = [{ Path = path1; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }; ]}
    let fsVs2 = { Identifier = "var_log_mysql_s"; Paths = [{ Path = path2; IsRecursive = true; IsAddition = true; IsSticky = Some(false); IsDirectory = true; }; ]}
    let rules = [{Subject = subject; Object = fsVs1; Permissions = VirtualSpacePermissions.Read}; { Subject = subject; Object = fsVs2; Permissions = VirtualSpacePermissions.Write }]

    let result = PolicyMining.mergeRules rules
    result.Length.ShouldBe 2
    result.ShouldBe rules