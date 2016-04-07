// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open System.Globalization
open System.Reflection
open FSharp.Data
open FSharp.Data.JsonExtensions
open System.Collections.Generic

open Game

let dataUri = "http://opendata.technolution.nl/opendata/parkingdata/v1/dynamic/23f58b84-8905-474e-b0de-366a3fd4457a";


[<EntryPoint>]
let main argv = 
//    let jsonData = JsonValue.AsyncLoad(dataUri, CultureInfo.GetCultureInfo("en-EN"))
//
//    let rec updateReq newData = 
//        System.Threading.Thread.Sleep(5000)
//        match newData with 
//        | JsonValue.Array [| info; data |] ->
//                    printfn "%A" info?parkingFacilityDynamicInformation
//                    updateReq (jsonData |> Async.RunSynchronously)
//        | JsonValue.Array xs ->
//                    printfn "%A" xs
//                    updateReq (jsonData |> Async.RunSynchronously)
//        | JsonValue.Record [| info; data |] ->
//                    printfn "%A" info
//                    updateReq (jsonData |> Async.RunSynchronously)
//        | JsonValue.String data ->
//                    printfn "%A" data
//                    updateReq (jsonData |> Async.RunSynchronously)
//        | data ->   printfn "Current free spots: %A" data?parkingFacilityDynamicInformation?facilityActualStatus?lastUpdated
//                    updateReq (jsonData |> Async.RunSynchronously)
//        | _ ->      printfn "failed"
//                    updateReq (jsonData |> Async.RunSynchronously)
//
//    updateReq (jsonData |> Async.RunSynchronously)
    use g = new TrainSimulation()
    g.Run()
    0 // return an integer exit code
