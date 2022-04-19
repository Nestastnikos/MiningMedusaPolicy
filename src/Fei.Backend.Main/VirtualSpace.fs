module VirtualSpace

open VirtualSpaceTypes
open System

let convertToPermissions (input:string) =
  input.ToCharArray()
  |> Array.map (fun x ->
    match x with
    | 'r' -> VirtualSpacePermissions.Read
    | 'w' -> VirtualSpacePermissions.Write
    | 's' -> VirtualSpacePermissions.See
    | _ -> raise (ArgumentException("Unknown permission - " + string x)))
  |> Array.reduce (fun a b -> a+b)

let permissionsToString (permissions: VirtualSpacePermissions) =
  match int permissions with
  | 1 -> "r"
  | 2 -> "w"
  | 3 -> "rw"
  | 4 -> "s"
  | 5 -> "rs"
  | 6 -> "ws"
  | 7 -> "rws"
  | _ -> raise (ArgumentException("Unknown permissions - " + string permissions))