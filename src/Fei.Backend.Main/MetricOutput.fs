module MetricOutput

open VirtualSpaceTypes

type OutputMetric = { NumOfEntries: int; NumOfRules: int; Wsc: float; Phase: string; }

let setupMetricComputation wSubj wObj wRules =
  let computeWsc rules =
    let subjectWsc =
      rules
      |> List.map(fun r -> r.Subject)
      |> List.distinct
      |> List.map (fun _ -> 2)
      |> List.sum
      |> float

    let objectWsc =
      rules
      |> List.map(fun r -> 1 + (r.Object.Paths |> List.length))
      |> List.sum
      |> float

    let ruleWsc = (rules |> List.length) * 3 |> float
    (wSubj * subjectWsc) + (wObj * objectWsc) + (wRules * ruleWsc)

  let computeMetrics numEntries phase rules =
    { NumOfEntries = numEntries;
      NumOfRules = rules |> List.length;
      Wsc = rules |> computeWsc;
      Phase = phase; }

  computeMetrics

