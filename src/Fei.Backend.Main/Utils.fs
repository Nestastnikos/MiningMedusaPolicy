module Utils

  let merge maps =
    maps
    |> Seq.fold
      (fun (state:Map<'a,'b>) map ->
        Map.fold (fun acc key value -> acc |> Map.add key value) state map)
      (Map([]))
