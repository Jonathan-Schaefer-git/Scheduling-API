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


let readInJson() =
    let settings = JsonSerializerSettings()
    settings.FloatParseHandling <- FloatParseHandling.Double
    JsonConvert.DeserializeObject<Problem>(File.ReadAllText("info.json"), settings)


let parseJsonBody (req: HttpRequest) =
    let settings = JsonSerializerSettings()
    settings.FloatParseHandling <- FloatParseHandling.Double
    JsonConvert.DeserializeObject<Problem>(req.rawQuery, settings)


let apiRoutes =
    choose [
        GET >=> choose [
            path "/api/version" >=> OK (sprintf "%s" ModularisedScheduling.version)
        ]
        POST >=> choose [
            path "/api/solve" >=> 
                request (fun req -> 
                        OK (sprintf "%A" (req |> parseJsonBody |> constructProblem))
                    )
        ]
        NOT_FOUND "No appropriate handler found"
    ]


[<EntryPoint>]
let main argv =

    //let myCfg =
    //    { Suave.Web.defaultConfig with
    //        bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" 8080 ]
    //    }

    //let app = choose [apiRoutes; NOT_FOUND "Route not found."]
    //startWebServer myCfg app
    readInJson() |> ModularisedScheduling.constructProblem |> printfn "%A"
    0 // Return an integer exit code
