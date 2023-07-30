open ModularisedScheduling
open Suave
open System
open Expecto
open System.IO
open Suave.Http
open Suave.Filters
open Suave.Operators
open Newtonsoft.Json
open Suave.Successful
open Suave.RequestErrors
open Suave.Authentication
open Newtonsoft.Json.Converters


let jsonToProblem json =
    try
         json 
        |> JsonConvert.DeserializeObject<Problem>
        |> Some 
    with
        | _ -> None



let apiRoutes =
    choose [
        GET >=> choose [
            path "/api/version" >=> OK (sprintf "%s" version)
        ]
        POST >=> choose [
            path "/api/solve" >=> 
                request (fun req -> 
                        
                        match req.form.[0] |> fst |> jsonToProblem with
                        | Some problem -> 
                            printfn "%s" req.rawQuery
                            OK (sprintf "%A" (problem |> constructProblem))
                        | None -> BAD_REQUEST "The JSON submitted was found to be invalid. Try JSON akin to the wiki one"
                    )
        ]
        NOT_FOUND "No appropriate handler found. Refer to the source code for the appropriate handlers"
    ]


[<EntryPoint>]
let main argv =

    let myCfg =
       { Suave.Web.defaultConfig with
           bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" 8080 ]
       }

    let app = choose [apiRoutes; NOT_FOUND "Route not found."]
    startWebServer myCfg app

    0 // Return an integer exit code
