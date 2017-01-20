module KMeans.Data

open System

let [<Literal>] DataSet =
    "http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"

let getDataFromFile() =
    System.IO.File.ReadAllLines("Data.csv")


let classes, data =
    use client = new System.Net.WebClient()
    client.DownloadString(DataSet).Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.skip 1
    |> Array.map (fun line ->
        let x = line.Split(';') |> Array.map (float)
        let _class = int(x.[x.Length-1])
        let features = Array.sub x 0 (x.Length-1)
        _class, features)
    |> Array.unzip

let dist:(float[] -> float[] -> float) =
    Array.fold2 (fun x u v -> x + pown (u - v) 2) 0.0

let getRandomCentroids =
    let seed = (int) DateTime.Now.Ticks
    let rnd = System.Random(seed)
    (fun count (data:float[][]) ->
        Array.init count (fun _ -> data.[rnd.Next(data.Length)]))
