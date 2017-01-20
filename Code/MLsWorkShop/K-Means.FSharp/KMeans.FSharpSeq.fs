module KMeans.FSharpSeq

let MaxIterations =1000

let kmeans (data:float[][]) dist initialCentroids =
    let N = data.[0].Length

    // find the nearest centroid by the min distance
    // the Array.minBy as signature (('a -> 'b) -> 'a [] -> 'a)
    // in this case the function dist is partially applied to the
    // u float array value, which will be used to pass each row
    // from the data
    let nearestCentroid centroids u =
        Array.minBy (dist u) centroids


    // Step 3
    // challenge : can you re-implement the rest of the code
    // using the C# implementation as reference

    let updateCentroids (centroids) =
        // clusterize data points using centroids
        // -- group the data by closest centroid
        data
        |> Seq.groupBy (nearestCentroid centroids)
        // use each cluster found (the values of the grouping)
        // to update the centroids. The new centroid will
        // be mean point of new clusters (groups)
        //      - for each cluster initialize an array
        //        of size N with each value as the average
        //        of each row of the cluster

        // [ YOUR CODE GOES HERE! ]

        // Order the centroids (required for error calculation)
//        |> Seq.sort
//        |> Seq.toArray
        [||]  // TODO Remove this line and uncomment the previous 2

    // update the centroids until they move but not more than n iteration (n = 1000)
    // for each iteration the centroids classification is optimized
    let rec update n centoids =
        // evaluate the updated centroids
        let newCentroids = updateCentroids centoids
        // found the total error in the form
        // of sum of the distances between
        // the current centroids and the updated ones
        let error = 0.0
            // [ YOUR CODE GOES HERE! ]

        // repeat the iteration for the updated centroids
        // if the centrods still move (total error is bigger than 1e-9 (zero))
        // and we do not reached iteration limit
        if n=0 || (error < 1e-9)
        then printfn "Iterations %d" (MaxIterations-n)
             newCentroids
        else update (n-1) newCentroids
    update MaxIterations initialCentroids


// Exercise implement different error function :
// current implementation uses distance between old and new centroids (that why we need to sort them)
// and when the sum distance is close to 0 we claim that centroids is stable and does not make sense to iterate further

// exercise:
//  error = sum of distances between each point and closest centroid.
//  stop condition = error does not (almost) change between iterations


