open System.IO
open System.Text.RegularExpressions


let parseRawFields line =
  let pattern = "\\w+=[\\w:\\/.,]+(\\([\\w\\/\\s:.]+\\)){0,1}"
  Regex.Matches(line, pattern)


let parseToMap (fields: MatchCollection) =
  fields
    |> Seq.map(fun x -> x.Value.Split "=")
    |> Seq.fold (fun (state:Map<string,string>) x -> state.Add (x.[0], x.[1])) (Map([]))


let parseIdentifier field =
  let pattern = "(?<=:)[0-9]+(?=\\))"
  (Regex.Match (field, pattern)).Value


let merge maps =
  maps
  |> Seq.fold
    (fun (state:Map<'a,'b>) map ->
      Map.fold (fun acc key value -> acc |> Map.add key value) state map)
    (Map([]))


"./Fei.Backend.Main/Resources/example-mysqld_t.log"
  |> File.ReadAllLines
  |> Seq.filter (fun l -> not (l.StartsWith "----"))
  |> Seq.map (fun l -> l |> parseRawFields |> parseToMap)
  |> Seq.map (fun m -> m |> Map.add "id" (parseIdentifier m.["msg"]))
  |> Seq.groupBy (fun m -> m.["id"])
  |> Seq.map (fun (k,maps) -> (k, maps |> merge))
  |> Seq.iter (printfn "%A")