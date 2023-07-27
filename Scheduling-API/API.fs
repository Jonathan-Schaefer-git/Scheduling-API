open ModularisedScheduling
open Suave
open System
open Expecto
open Suave.Http
open Suave.Filters
open Suave.Operators
open Newtonsoft.Json
open Suave.Successful
open Suave.RequestErrors
open Suave.Authentication
open Newtonsoft.Json.Converters



let solveIssue (json:string) =
    0

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
                        req |> parseJsonBody
                    )
        ]
        NOT_FOUND "No appropriate handler found"
    ]


[<EntryPoint>]
let main argv =

    let myCfg =
        { Suave.Web.defaultConfig with
            bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" 2000 ]
        }

    let app = choose [apiRoutes; NOT_FOUND "Route not found."]
    startWebServer myCfg app
    0 // Return an integer exit code
