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
            path "/api/hello" >=> OK "Hello, Suave!"
            pathScan "/api/greet/%s" (fun name -> OK (sprintf "Hello, %s!" name))
        ]
    ]


[<EntryPoint>]
let main argv =
    //let app = choose [apiRoutes; NOT_FOUND "Route not found."]
    //startWebServer Suave.Web.defaultConfig app
    solve_test()
    0 // Return an integer exit code
