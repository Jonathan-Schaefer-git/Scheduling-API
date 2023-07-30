module Protocol
open FSharp.Data
open FSharp.Data.CsvExtensions
open System.IO

type Stats = {
    workers:int
    shifts:int
    weeks:int
    time:int64
}


let writeProtocol (stats:Stats) : unit =
    File.AppendAllText("/home/jona/Desktop/Scheduling-Testing/Datapoints/scheduling-data.csv", (sprintf "%s,%s,%s,%s" (stats.workers.ToString()) (stats.shifts.ToString()) (stats.weeks.ToString()) (stats.time.ToString())))
   