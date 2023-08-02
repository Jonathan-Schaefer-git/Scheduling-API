module ModularisedScheduling

open Protocol
open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure
open System.Diagnostics

[<Measure>] type Euro
[<Measure>] type Hour
[<Measure>] type Strain
[<Measure>] type Worker
[<Measure>] type Shift

type Options = {
    expenseMinimizing:bool
    strainMinimizing:bool
    ensureQualifiedPersonellConstraint:bool
    noDoubleShiftConstraint:bool
    capMaximumWorkingHoursConstraint:bool
}

//! Domain Model

// Every day has a certain amount of time slots (default: 3) which can each contrain 0, 1 or more shifts which need to be staffed


type Employee = {
    Name:string
    Occupation:string
    Wage:float<Euro/Hour>
}


type ShiftInfo = {
    Name: string
    Length: float<Hour/Shift>
    RequiredPersonal: (int<Worker/Shift> * string) list
    Strain: float<Strain/Shift>
}

type TimeSlot = { shifts:ShiftInfo list}

type Day = { timeSlots: TimeSlot list }

type Week = { days:Day list }

type Schedule = { weeks:Week list }

type Problem = {
    workers:Employee list
    schedule:Schedule
    maxHoursPerWeek:float<Hour>
    options:Options
}

let problemToProtocol problem (stopwatch:Stopwatch) (success:bool) : unit =
    let shiftsPerWeek = [for week in problem.schedule.weeks do [for day in week.days do [for timeslot in day.timeSlots do [for shift in timeslot.shifts -> 1] |> List.sum ] |> List.sum ] |> List.sum ] |> List.sum
    {workers=problem.workers.Length; shifts=shiftsPerWeek; weeks=problem.schedule.weeks.Length; time=stopwatch.ElapsedMilliseconds; success=success} |> writeProtocol


let version = "beta-1.0.3"
let features = 
    [
        "Dynamic schedule supported"
        "Supports concurrent shifts per time slot"
    ]

let constructProblem (problem:Problem) =

    // Helper variables to make the code more readable
    let workers = problem.workers
    let schedule = problem.schedule
    let maxHoursPerWeek = problem.maxHoursPerWeek

    let workersWage =
        [for record in workers -> record, record.Wage] |> SMap.ofList

    // Here are the shifts helpers defined
    let shiftLength = 
        [
            for x=0 to schedule.weeks.Length - 1 do
                for y=0 to schedule.weeks.[x].days.Length - 1 do
                    for z=0 to schedule.weeks.[x].days.[y].timeSlots.Length - 1 do
                        for shift in schedule.weeks.[x].days.[y].timeSlots.[z].shifts ->
                            (x,y,z,shift),shift.Length
        ] |> SMap4.ofList



    let strainOfShifts = 
        [
            for x=0 to schedule.weeks.Length - 1 do
                for y=0 to schedule.weeks.[x].days.Length - 1 do
                    for z=0 to schedule.weeks.[x].days.[y].timeSlots.Length - 1 do
                        for shift in schedule.weeks.[x].days.[y].timeSlots.[z].shifts ->
                            (x,y,z,shift),shift.Strain
        ] |> SMap4.ofList


    // Builds a binary matrix per worker of 3 shifts (as columns) and 7 days (as Rows) for every employee
    //! Decision
    let shouldWork =
        DecisionBuilder<Shift> "Has to work" {
            for employee in workers do
                for x=0 to schedule.weeks.Length - 1 do
                    for y=0 to schedule.weeks.[x].days.Length - 1 do
                        for z=0 to schedule.weeks.[x].days.[y].timeSlots.Length - 1 do
                            for shift in schedule.weeks.[x].days.[y].timeSlots.[z].shifts ->
                                Boolean
        } |> SMap5.ofSeq
        
    //! Constraints

    // Ensures sufficient, qualified staffing
    let qualifiedConstraints =
        ConstraintBuilder "Ensure qualified personell and enough of workers of in shift" {
            for x=0 to schedule.weeks.Length - 1 do
                for y=0 to schedule.weeks.[x].days.Length - 1 do
                    for z=0 to schedule.weeks.[x].days.[y].timeSlots.Length - 1 do
                        for shift in schedule.weeks.[x].days.[y].timeSlots.[z].shifts do
                            for (reqWorkers, qualification) in shift.RequiredPersonal ->
                                sum(shouldWork.[Where (fun employee -> employee.Occupation = qualification),x,y,z,shift]) >== float(reqWorkers) * 1.0<Shift>
        }

    // Maximum worktime per week
    let maxHoursConstraints =
        ConstraintBuilder "Maximum Constraint" {
            for employee in workers do
                for week=0 to schedule.weeks.Length - 1 do
                    yield sum (shouldWork.[employee,week,All,All,All] .* shiftLength.[week,All,All,All]) <== maxHoursPerWeek
        }
    
    // No double shift on one day can be worked
    let noDoubleShiftConstraint =
        ConstraintBuilder "No Double Shift Constraint" {
            for employee in workers do
                for x=0 to schedule.weeks.Length - 1 do
                    for y=0 to schedule.weeks.[x].days.Length - 1 do
                        yield sum(shouldWork.[employee,x,y,All,All]) <== 1.0<Shift>
        }

    //! Objectives
    let minimizeStrain =
        [
            for employee in workers do
                sum (shouldWork.[employee,All,All,All,All] .* strainOfShifts)
        ]
        |> List.sum
        |> Objective.create "Minimize strain on workers" Minimize

    let minimizeCosts =
        [
            for employee in workers do
                for x=0 to schedule.weeks.Length - 1 do
                    for y=0 to schedule.weeks.[x].days.Length - 1 do
                        for z=0 to schedule.weeks.[x].days.[y].timeSlots.Length - 1 do
                            for shift in schedule.weeks.[x].days.[y].timeSlots.[z].shifts ->
                                shouldWork.[employee,x,y,z,shift] * shiftLength.[x,y,z,shift] * workersWage.[employee]
        ]
        |> List.sum
        |> Objective.create "Minimize Cost Target" Minimize

    //todo Implement a way to minimize shift switches
    //note Maybe minimize cross product? As it is a matrix?

    let retrieveSolutionValues (result:SolveResult) (stopwatch:Stopwatch) =
        match result with
        | Optimal solution ->
            let shiftsPerWeek = [for day in schedule.weeks.[0].days do [for timeSlot in day.timeSlots -> timeSlot.shifts.Length] |> List.sum ] |> List.sum
            {workers=workers.Length; shifts=shiftsPerWeek * schedule.weeks.Length; weeks=schedule.weeks.Length; time=stopwatch.ElapsedMilliseconds; success=true} |> writeProtocol
            let values = Solution.getValues solution shouldWork |> SMap5.ofMap
            [
                for week=0 to schedule.weeks.Length - 1 do 
                [
                    for day=0 to schedule.weeks.[week].days.Length - 1 do
                    [
                        for timeslot=0 to schedule.weeks.[week].days.[day].timeSlots.Length - 1 do
                        [
                            for shift in schedule.weeks.[week].days.[day].timeSlots.[timeslot].shifts do
                            [
                                let x = values.[All,week,day,timeslot,shift]
                                for employee in workers do
                                    if x.[employee] = 1.0<Shift> then yield employee.Name
                            ]
                        ]
                    ]
                ]
            ]
        | _ -> 
            let shiftsPerWeek = [for day in schedule.weeks.[0].days do[for timeSlot in day.timeSlots -> timeSlot.shifts.Length] |> List.sum ] |> List.sum
            {workers=workers.Length; shifts=shiftsPerWeek * schedule.weeks.Length; weeks=schedule.weeks.Length; time=stopwatch.ElapsedMilliseconds; success=true} |> writeProtocol
            [[[[[sprintf "[Error]: Model infeasible -> %A" result]]]]]


    // Prepare for stats extraction
    let stopwatch = Stopwatch.StartNew()
    //! Solve model
    let solved = 
        let options = problem.options


        let mutable model = 
            match (options.expenseMinimizing, options.strainMinimizing) with
            | true, false -> Model.create minimizeCosts
            | false, true -> Model.create minimizeStrain
            | _ -> 
                Model.create minimizeCosts
                |> Model.addObjective minimizeStrain

        if options.ensureQualifiedPersonellConstraint then
            model <- Model.addConstraints qualifiedConstraints model
        if options.noDoubleShiftConstraint then
            model <- Model.addConstraints noDoubleShiftConstraint model
        if options.capMaximumWorkingHoursConstraint then
            model <- Model.addConstraints maxHoursConstraints model

        model
        |> Solver.solve Settings.basic
        
    stopwatch.Stop()
    retrieveSolutionValues solved stopwatch


//! Unit test
let testCase() =
    let shifts = 
       [    
           {Name="Morning Shift"; RequiredPersonal=[(1<Worker/Shift>, "EMT"); (1<Worker/Shift>,"Doctor")];                             Length=8.0<Hour/Shift>;    Strain=1.2<Strain/Shift>}
           {Name="Late Shift";    RequiredPersonal=[(1<Worker/Shift>, "EMT"); (1<Worker/Shift>,"Doctor"); (1<Worker/Shift>, "Nurse")]; Length=8.0<Hour/Shift>;    Strain=1.0<Strain/Shift>}
           {Name="Night Shift";   RequiredPersonal=[(1<Worker/Shift>, "Doctor")];                                                      Length=8.0<Hour/Shift>;    Strain=1.8<Strain/Shift>}
       ]

    let workers = 
       [
           {Name="Jenna";    Occupation = "EMT";     Wage=25.0<Euro/Hour>}
           {Name="Hannah";   Occupation = "Nurse";   Wage=20.0<Euro/Hour>}
           {Name="George";   Occupation = "Doctor";  Wage=30.0<Euro/Hour>}
           {Name="Freddy";   Occupation = "Doctor";  Wage=31.0<Euro/Hour>}
           {Name="Kiley";    Occupation = "Doctor";  Wage=28.0<Euro/Hour>}
           {Name="Delta";    Occupation = "EMT";     Wage=24.0<Euro/Hour>}
           {Name="Marlee";   Occupation = "Doctor";  Wage=34.0<Euro/Hour>}
           {Name="Tucker";   Occupation = "Nurse";   Wage=18.0<Euro/Hour>}
           {Name="Lawrence"; Occupation = "EMT";     Wage=25.0<Euro/Hour>}
       ]

    let simplexschedule =
        {
            weeks=
                [{
                    days=
                    [
                        {
                            timeSlots=
                            [
                                {shifts=[shifts.[0]]}
                                {shifts=[shifts.[1]]}
                                {shifts=[shifts.[2]]}
                            ]
                        };
                        {
                            timeSlots=
                            [
                                {shifts=[shifts.[0]]}
                                {shifts=[shifts.[1]]}
                                {shifts=[shifts.[2]]}
                            ]
                        };
                        {
                            timeSlots=
                            [
                                {shifts=[shifts.[0]]}
                                {shifts=[shifts.[1]]}
                                {shifts=[shifts.[2]]}
                            ]
                        };
                        {
                            timeSlots=
                            [
                                {shifts=[shifts.[0]]}
                                {shifts=[shifts.[1]]}
                                {shifts=[shifts.[2]]}
                            ]
                        };
                        {
                            timeSlots=
                            [
                                {shifts=[shifts.[0]]}
                                {shifts=[shifts.[1]]}
                                {shifts=[shifts.[2]]}
                            ]
                        };
                        {
                            timeSlots=
                            [
                                {shifts=[shifts.[0]]}
                                {shifts=[shifts.[1]]}
                                {shifts=[shifts.[2]]}
                            ]
                        };
                        {
                            timeSlots=
                            [
                                {shifts=[shifts.[0]]}
                                {shifts=[shifts.[1]]}
                                {shifts=[shifts.[2]]}
                            ]
                        };

                    ]
                }]
        }

    {workers=workers;schedule=simplexschedule;maxHoursPerWeek=50.0<Hour>;options={expenseMinimizing=true;strainMinimizing=true;capMaximumWorkingHoursConstraint=true;ensureQualifiedPersonellConstraint=true;noDoubleShiftConstraint=true}}