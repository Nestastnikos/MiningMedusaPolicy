module InputMapping


open DomainTypes
open System

type InputPathEntryDto = {
  FullPath: string;
  IsAddition: bool;
  IsSticky: bool;
  IsRecursive: bool;
  Type: string; }

type InputObjectDto = {
  Identifier: string;
  Paths: InputPathEntryDto list;
}

type InputRuleDto = {
  SubjectUid: string;
  SubjectProctitle: string;
  Object: InputObjectDto;
  Permissions: string }


let ruleDtoToRule dto =
  let subject = (dto.SubjectUid, dto.SubjectProctitle)

  let pathEntries =
    dto.Object.Paths
    |> List.map (fun x ->
      match x.Type with
      | "FILE" ->
        File({ FullPath = x.FullPath; IsAddition = x.IsAddition })
      | "DIR" ->
        Directory({ FullPath = x.FullPath; IsAddition = x.IsAddition; IsSticky = x.IsSticky; IsRecursive = x.IsRecursive })
      | _ ->
        raise (ArgumentException("Unknown virtual space type")))
  let object = (dto.Object.Identifier, pathEntries)

  let permissions =
    dto.Permissions.ToCharArray()
    |> Array.map (fun x ->
      match x with
      | 'r' -> Read
      | 'w' -> Write
      | 's' -> See
      | _ -> raise (ArgumentException("Unknown virtual space value")))
    |> Array.toList
  { Subject = subject; Object = object; Permissions = permissions }