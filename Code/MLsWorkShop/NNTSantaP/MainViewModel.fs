namespace ViewModels

open System
open System.Threading
open FSharp.ViewModule
open FSharp.Charting
open FSharp.Charting.ChartTypes
open System.Windows.Forms.Integration
open FSharp.Control.Reactive

[<AutoOpen>]
module Utilities =
    let [<Literal>] rangeLen = 1000.
    let rand = new Random(int DateTime.Now.Ticks)

    type RandMessage =
        | GetMap of int * AsyncReplyChannel<(float * float)[]>
        | SetMap of int * AsyncReplyChannel<(float * float)[]>

    let mapAgent =
        MailboxProcessor.Start(fun inbox ->
            let initMap n = Array.init n (fun _ -> rand.NextDouble()*rangeLen, rand.NextDouble()*rangeLen)
            let rec loop (map:(float*float)[]) =
                async {
                    let! msg = inbox.Receive()
                    match msg with
                    | GetMap(n, reply) ->
                        let map =
                            match map with
                            | x when n = x.Length -> map
                            | _ -> initMap n
                        reply.Reply(map)
                        return! loop map
                    | SetMap(n, reply) ->
                        let map = initMap n
                        reply.Reply(map)
                        return! loop map
                }
            loop (initMap 0))



// Step .1
// lets start by defining the model representing the Neuron
// the neuron in this case has only 2 properties
//      - weights as an array of floats. (technically it could be a point with coordinate X Y, for simplicity we are using an array)
//      - output as a single float, which is the result of the computation
//
//  the Neuron can be defined as a RecordType in F#
//      for example, a Record type for a Person is
//          type Person = {firstName:string; lastName:string }
//      to create a record type simply
//              let person = {firstName="Riccardo";  lastName="Terrell" }
//      the F# type inference will do the rest


// [ YOUR CODE GOES HERE! ]

// should be helpful to have 2 functions to access the properties
// of a Neuron instance
// for example to access the length of the weights use this function (uncomment)
//    member this.inputsCount = this.weights.Length


//    then create a function that access an item of the Neuron weight array
//    with the following initial definition

//    member this.item n = [ code here ]



// Step .2
// create 2 functions helpers for the Neuron
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix )>]
module Neuron =
    let create weight' =
        // the create function returns a default Neuron RecordType with
        // output zero and weight as the weight' passed
        // [ YOUR CODE GOES HERE! ]

    // create a Neuron with random weight
    let createRandom (inputs : int) =
        create <| Array.init (max 1 inputs) (fun _ -> rand.NextDouble() * rangeLen)


    let compute (neuron : Neuron) (input : float[]) =
        // create a function that return an array of float
        // each value is the sum of the pairs between the value of
        // the item from the Neuron Weight and the input item having same indexes
        //      for example
        //      let arr1 = [|1;2|]
        //      let arr2 = [|3;4|]
        //      the result is arr3 = [|4;6|]
        //
        // F# has a great support for sequence manipulation
        //      for example Seq.map   Seq.filter ...
        //      in this case, beside a for-loop you could try to
        //      Seq.zip    and    Seq.sumBy


        // [ YOUR CODE GOES HERE! ]


    // for testing, using the same function (passing a float array instead of the weights-neuron)
    // with the following dummy data the result is 980.0
    // uncomment and test after have created the fucntion
//    let test() =
//        let a = [1.0..20.]
//        let b = [50. ..70.]
//        compute a b

// Step .3
// taking as example the Neuron record-type, lets repeat the same approach
// to define a Layer type
// a Layer has 2 properties, a neurons property to access an array of Neurons
// and an array of float for the output

// in addition add 2 instance function for the Layer Record-Type
//      1 - get the counts on Neurons
//      2 - get a neuron given an index n

/// A layer represents a collection of neurons
// [ YOUR CODE GOES HERE! ]



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Layer =

// Step .4
// define a function to create a Layer type
// the arguments are the integer for count of Neurons,
// and a function used to initialize each Neuron with signature (int -> Neuron)
//
//      in F# is handy the Array module.
//      for example, to initialize an Array with a give function you could use the Array.init function
//                   that takes as argument the count of Neurons
//                   and a function to create each item  (int -> (int -> 'a)) where 'a is a Neuron type
//                   for example, to create an array of string you can use the following code
//                                for simplicity the string is the result of converting each array-index number to string (but you get the idea :) )//
//
//                                Array.int 100 (fun x -> string x)
//
//                                in F# everything is a function, the 'string' function take a generic type 'a and converted into a string
//                                thus, you could rewrite the previous code as follow
//
//                                Array.init 100 (string)
//
//                  to create an array with default values for the output there several options, try to found out what the Array module can do

    let create neuronsCount ctorFunc =
        {
         // [ YOUR CODE GOES HERE! ]
        }

    // Create Layer with shape of bubble with `R` radius center in `c` point
    let createBubble neuronsCount (c:float[]) R =
        let neuronsCount = max 1 neuronsCount
        let delta = Math.PI * 2.0 / (float neuronsCount)
        let initFunc i =
            let alpha = float(i) * delta
            let x' = c.[0] + R * Math.Cos(alpha)
            let y' = c.[1] + R * Math.Sin(alpha)

            // add the function to create a Neuron early defined
            // -  the argument is an array of float, which in this case are just the coordinate of the Neuron x y
            // [ YOUR CODE GOES HERE! ]

        create neuronsCount initFunc

    // Create Layer with random neuron location
    let createRandom neuronsCount inputsCount  =
        create neuronsCount (fun i -> Neuron.createRandom inputsCount)

    /// Compute output vector of the layer
    let compute (inputs : float array) (layer : Layer) =
        let neuronsCount = layer.neuronsCount
        let output = Array.init neuronsCount (fun i -> Neuron.compute layer.neurons.[i] inputs)
        { layer with output = output }



// Step .5 Create Single hidden layer called Network, which is just an alias for the Layer :)
type Network = Layer // Single hidden layer


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Network =
    let createBubble = Layer.createBubble
    let zero = Layer.createRandom 0 2

    let createRandom inputsCount neuronsCount =
        Layer.createRandom neuronsCount inputsCount

    let compute (network : Network) (input : float array) =
        network |> Layer.compute input

// Step .5 Create Single hidden layer called Network, which is just an alias for the Layer type :)
// [ YOUR CODE GOES HERE! ]

//  complete the findBestOutput function
//  from the output array in the Network type
//  found the best value. However, is not that simple :)
//  you should create a collection composed by the tuple of each value of the output array
//  and its index in the array.
//  for example. an array of [|10;30|] would become [|(10,0); (30,1)|]  where 0 and 1 are the index
//  of the position of a give value in the array
//  the found the min value between the tuples and return its index value

    let findBestOutput (network : Network) =
    // [ YOUR CODE GOES HERE! ]



// Definition of the Elastic Network
type ElasticNetworkLearning =
    { learningRate : float
      learningRadius : float
      squaredRadius : float
      distance : float array
      network : Network }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NetworkLearning =
    let create (network : Network) =
        let neuronsCount = network.neuronsCount
        let delta = Math.PI * 2.0 / (float neuronsCount)

        let rec initDistance i alpha acc =
            match i with
            | n when n < neuronsCount ->
                let x = 0.5 * Math.Cos(alpha) - 0.5
                let y = 0.5 * Math.Sin(alpha)
                initDistance (i + 1) (alpha + delta) ((x * x + y * y)::acc)
            | _ -> acc |> List.toArray
        // initial arbitrary values
        { learningRate = 0.1
          learningRadius = 0.5
          squaredRadius = 98.
          distance = initDistance 0 delta []
          network = network }

    let setLearningRate learningRate (learning : ElasticNetworkLearning) =
        { learning with learningRate = max 0. (min 1. learningRate) }

    let setLearningRadius learningRadius (learning : ElasticNetworkLearning) =
        let learningRadius = max 0. (min 1. learningRadius)
        { learning with learningRadius = learningRadius
                        squaredRadius = 2. * learningRadius * learningRadius }

    let compute (learning : ElasticNetworkLearning) (input : float array) =

// Step .6 A
// lets put all together to update a given ElasticNetworkLearning
        let learningRate = learning.learningRate

        let network =  // Compute the Network using the compute function passing the network
                       // from the current ElasticNetworkLearning and the new input values
                       Network.compute learning.network input
        let bestNeuronId = // get the best output (using the findbestoutput) from the early network computed
                           Network.findBestOutput network
        let layer = network // Just single hidden layer in network


// Step .6 B
// now is time to return a new ElasticNetworkLearning with same values as the (learning : ElasticNetworkLearning)
// argument but with an update network value
        for j = 0 to layer.neuronsCount - 1 do
        // [ YOUR CODE GOES HERE! ]
        // take a neuron from the layer value using the helper function to access a neuron
        // with a given index, in this case j

        //  calculate the factor value , which is the 'exp' of the negative number of the
        //  distance value of the ElasticNetworkLearning (you can access this value with the learning.distance property)
        //          -> having as index the 'abs' value of the current index j minus the best-NeuronId
        //  divide by the squaredRadius value of the ElasticNetworkLearning
        //
        //  for example
        //      let factor = exp (- learning.distance.[abs .... more code here ]
        //
        // then update each neuron weights
        //      the new neuron weight is the sum of its own current value
        //      plus the learningRate
        //      plus the difference between the value take it from the input array and the neuron weight
        //          both having same index in the array (good solution is a for loop and using the same index to access both array)
        //          this difference  (input.[index] - neuron.... ) multiplied by the factor value early computed
        //
        //  return a new (learning : ElasticNetworkLearning) value with the updated network value
        //      in F# Record-Type are immutable. You can create a new Record-Type from an existing one with different
        //      property(ies) using the `with` keyword
        //          for example, to create a new record type from an existing `person` one but only with the `age` property different
        //                      let newPerson = { person with age = 42 }
        //      help: this is how mutate a value in an array in F#
        //                  neuron.weghts.[index] <- new value here
        //
        // [ YOUR CODE GOES HERE! ]
            let neuron = layer.item j
            let factor = exp (-learning.distance.[abs (j - bestNeuronId)] / learning.squaredRadius)
            for i = 0 to neuron.inputsCount - 1 do
                let e = (input.[i] - neuron.item i) * factor
                neuron.weights.[i] <- neuron.weights.[i] + (e + learningRate)
        { learning with network = network }




type ISantaSolver =
    // Execute: NumberOfIteraction -> uiUpdateFunc -> Final NN state
    abstract member Execute : int -> (int -> int -> Network -> Async<unit> ) -> Async<Network>


module TravelingSantaProblem =

    type ElasticSolver(clusterId:int, network:Network, learningRate:float, cities:float[][]) =
        static member NetworkFromCities cities neurons = Network.createRandom 2 neurons
        static member ctor learningRate (id, network, cities) = ElasticSolver(id, network, learningRate, cities) :> ISantaSolver

        interface ISantaSolver with
          override __.Execute iterations updateUI =
            async {
                let trainer = NetworkLearning.create network
                let fixedLearningRate = learningRate / 20.
                let driftingLearningRate = fixedLearningRate * 19.
                let iterations = float iterations

                for i = 0 to (int iterations - 1) do

                    let learningRateUpdated = driftingLearningRate * (iterations - float i) / iterations + fixedLearningRate
                    let trainer = NetworkLearning.setLearningRate learningRateUpdated trainer
                    let learningRadiusUpdated = trainer.learningRadius * (iterations - float i) / iterations
                    let trainer = NetworkLearning.setLearningRadius learningRadiusUpdated trainer

                    let input = cities.[i % cities.Length]
                    let trainer = NetworkLearning.compute trainer input

                    if i % 1000 = 0 then
                        do! updateUI clusterId (i-1) trainer.network

                do! updateUI clusterId (int iterations - 1) trainer.network

                return trainer.network
            }


open TravelingSantaProblem

type MainViewModel() as this =
    inherit ViewModelBase()


    let mutable cts = new CancellationTokenSource()

    let pointsStream = Event<(float * float)[]>()
    let livePointsChart =
        pointsStream.Publish
        |> Observable.map id
        |> LiveChart.Point


    let createChart pathes =
        let pathStreams = List.init pathes (fun _ -> Event<(float * float)[]>())
        let pathObs = pathStreams |> List.map (fun s -> s.Publish |> Observable.map(id))

        let livePathCharts = pathObs |> List.map (LiveChart.Line)
        let chartCombine = Chart.Combine(livePointsChart :: livePathCharts).WithYAxis(Enabled=false).WithXAxis(Enabled=false)

        let chart = new ChartControl(chartCombine)
        let host = new WindowsFormsHost(Child = chart)

        pathStreams, host


    let cities = this.Factory.Backing(<@ this.Cities @>, 100)
    let iterations = this.Factory.Backing(<@ this.Iterations @>, 25000)

    // To avoid oscillation of neurons between different cities, they proposed that
    // the number of neurons should be greater than number of cities (M >= 3N).
    // In our study we assume fixed number of neurons (M = 5N)
    let neurons = this.Factory.Backing(<@ this.Neurons @>, 4*100)
    let learningRate = this.Factory.Backing(<@ this.LearningRate @>, 0.005)
    let clusters = this.Factory.Backing(<@ this.Clusters @>, 4)

    let currentIterations = this.Factory.Backing(<@ this.CurrentIterations @>, 0)
    let executionTime = this.Factory.Backing(<@ this.ExecutionTime @>, "")
    do mapAgent.PostAndReply(fun ch -> SetMap(cities.Value, ch)) |> pointsStream.Trigger

    let mutable pathStreams, host = createChart 1
    let hostChart = this.Factory.Backing(<@ this.Chart @>, host)

    let initControls n =
        this.CurrentIterations <- 0
        this.ExecutionTime <- ""
        pointsStream.Trigger [||]
        mapAgent.PostAndReply(fun ch -> SetMap(n, ch)) |> pointsStream.Trigger
        pathStreams |> Seq.iter (fun stream -> stream.Trigger [||])

    let onCancel _ =
        this.CurrentIterations <- 0
        this.ExecutionTime <- ""
        pathStreams |> Seq.iter (fun stream -> stream.Trigger [||])
        cts.Dispose()
        cts <- new CancellationTokenSource()
        this.StartElasticCommand.CancellationToken <- cts.Token
        this.StartKniesCommand.CancellationToken <- cts.Token

    let cancelClear () =
        cts.Cancel()

    let cancel =
        this.Factory.CommandSyncChecked(cancelClear, (fun _ -> this.OperationExecuting), [ <@@ this.OperationExecuting @@> ])

    let initPoints =
        this.Factory.CommandSyncParamChecked(initControls, (fun _ -> not this.OperationExecuting), [ <@@ this.OperationExecuting @@> ])

    let createCommand getPathCount createSolver =
        this.Factory.CommandAsync((fun ui -> async {
            let streams, host = createChart (getPathCount())
            pathStreams <- streams
            hostChart.Value <- host

            let updateControl streamId currentIteration (network:Network) =
                async {
                    let path =
                      if network = Network.zero then [||]
                      else
                        Array.init (network.neuronsCount + 1) (fun id ->
                            let n = id % network.neuronsCount
                            (network.item n).item 0, (network.item n).item 1)

                    do! Async.SwitchToContext ui
                    this.CurrentIterations <- (currentIteration + 1)
                    streams.[streamId].Trigger path
                    do! Async.SwitchToThreadPool()
                }

            let time = System.Diagnostics.Stopwatch.StartNew()
            let! cities = mapAgent.PostAndAsyncReply(fun ch -> GetMap(cities.Value, ch))
            let cities = cities |> Array.map (fun (x,y) -> [|x;y|])

            let tsp = createSolver(cities) :> ISantaSolver
            let! _ = tsp.Execute iterations.Value updateControl

            this.ExecutionTime <- sprintf "Time %d ms" time.ElapsedMilliseconds
        }), token=cts.Token, onCancel=onCancel)

    let startElastic =
        createCommand
            (fun() -> 1)
            (fun cities ->
                let network = ElasticSolver.NetworkFromCities cities neurons.Value
                ElasticSolver(0, network, learningRate.Value, cities) )


    do initControls (cities.Value)

    member this.Chart
        with get () = hostChart.Value
        and set value = hostChart.Value <- value

    member this.Cities
        with get () = cities.Value
        and set value = cities.Value <- value

    member this.Neurons
        with get () = neurons.Value
        and set value = neurons.Value <- value

    member this.LearningRate
        with get () = learningRate.Value
        and set value = learningRate.Value <- value

    member this.Iterations
        with get () = iterations.Value
        and set value = iterations.Value <- value

    member this.Clusters
        with get () = clusters.Value
        and set value = clusters.Value <- value

    member this.CurrentIterations
        with get () = currentIterations.Value
        and set value = currentIterations.Value <- value

    member this.ExecutionTime
        with get () = executionTime.Value
        and set value = executionTime.Value <- value

    member this.InitPointsCommand = initPoints
    member this.StartElasticCommand : IAsyncNotifyCommand = startElastic
    member this.CancelCommand = cancel
