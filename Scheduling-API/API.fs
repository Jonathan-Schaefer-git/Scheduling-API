open ModularisedScheduling
open Suave
open Expecto
open Suave.Http
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.Authentication
open Suave.RequestErrors



let apiRoutes =
    choose [
        GET >=> choose [
            path "/api/version" >=> OK (sprintf "%s" ModularisedScheduling.version)
        ]
        POST >=> choose [
            path "/api/solve/{id}" >=> OK solve_test
        ]
        NOT_FOUND "No appropriate handler found"
    ]


[<EntryPoint>]
let main argv =
    ModularisedScheduling.solve_test()
    //let app = choose [apiRoutes; NOT_FOUND "Route not found."]
    //startWebServer Suave.Web.defaultConfig app
    0 // Return an integer exit code
