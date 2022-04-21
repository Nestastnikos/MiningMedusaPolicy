module DomainTypes


type FileEntry = { FullPath: string; IsAddition: bool; }
type DirEntry = { FullPath: string; IsAddition: bool; IsSticky: bool; IsRecursive: bool; }

type PathEntry =
  | File of FileEntry
  | Directory of DirEntry

type Uid = string
type Proctitle = string
type Identifier = string

type VirtualSpacePermission = Read | Write | See

type Rule = {
  Subject: Uid * Proctitle;
  Object: Identifier * PathEntry list
  Permissions: VirtualSpacePermission list
}
