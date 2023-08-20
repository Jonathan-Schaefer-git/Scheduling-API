module ModularisedScheduling

open Protocol
open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure
open System.Diagnostics

[<Measure>]
type Euro

[<Measure>]
type Hour

[<Measure>]
type Strain

[<Measure>]
type Worker

[<Measure>]
type Shift


// Challenge: Create a model that is able to create schedule that minimizes
// costs and strain on workers while respecting these constraints:
(*
    - No worker may work over 40 hours a week
    - No worker may work 2 shifts in one day
    - Each shift requires a specific amount of workers of a certain occupation
*)
// As well as minimizes possible code duplications and maximizes extensibility and modularity

type Options =
    { ExpenseMinimizing: bool
      StrainMinimizing: bool
      EnsureQualifiedPersonnelConstraint: bool
      NoDoubleShiftConstraint: bool
      MaximumWorkingHoursConstraint: bool
      MinimumWorkingHoursConstraint: bool }

//! Domain Model

// Every day has a certain amount of time slots (default: 3) which can each contrain 0, 1 or more shifts which need to be staffed


type Employee =
    { Name: string
      Occupations: string list
      Wage: float<Euro / Hour> }

type RequiredPersonnel =
    { Count: int<Worker / Shift>
      RequiredQualifications: string list }

type ShiftInfo =
    { Name: string
      Length: float<Hour / Shift>
      RequiredPersonnel: RequiredPersonnel list
      Strain: float<Strain / Shift> }

type TimeSlot = { Shifts: ShiftInfo list }

type Day = { TimeSlots: TimeSlot list }

type Week = { Days: Day list }

type Schedule = { Weeks: Week list }

type Problem =
    { Workers: Employee list
      Schedule: Schedule
      MaxHoursPerWeek: float<Hour>
      MinHoursPerWeek: float<Hour>
      Options: Options }

type Solution =
    { Status: bool
      Result: string list list list list list
      ObjectiveCost: float<Euro>
      ObjectiveStrain: float<Strain> }

let problemToProtocol problem (stopwatch: Stopwatch) (success: bool) : unit =
    let shiftsPerWeek =
        [ for week in problem.Schedule.Weeks do
              [ for day in week.Days do
                    [ for timeslot in day.TimeSlots do
                          [ for shift in timeslot.Shifts -> 1 ] |> List.sum ]
                    |> List.sum ]
              |> List.sum ]
        |> List.sum

    { workers = problem.Workers.Length
      shifts = shiftsPerWeek
      weeks = problem.Schedule.Weeks.Length
      time = stopwatch.ElapsedMilliseconds
      success = success }
    |> writeProtocol


let version = "beta-1.0.4"

let features =
    [ "Dynamic Schedule supported"
      "Supports concurrent shifts per time slot"
      "Mixed qualifications enabled for workers and shifts requirements" ]

let constructProblem (problem: Problem) =

    // Helper variables to make the code more readable
    let workers = problem.Workers
    let Schedule = problem.Schedule
    let maxHoursPerWeek = problem.MaxHoursPerWeek
    let minHoursPerWeek = problem.MinHoursPerWeek

    let workersWage = [ for record in workers -> record, record.Wage ] |> SMap.ofList

    // Here are the shifts helpers defined
    let shiftLength =
        [ for x = 0 to Schedule.Weeks.Length - 1 do
              for y = 0 to Schedule.Weeks.[x].Days.Length - 1 do
                  for z = 0 to Schedule.Weeks.[x].Days.[y].TimeSlots.Length - 1 do
                      for shift in Schedule.Weeks.[x].Days.[y].TimeSlots.[z].Shifts -> (x, y, z, shift), shift.Length ]
        |> SMap4.ofList



    let strainOfShifts =
        [ for x = 0 to Schedule.Weeks.Length - 1 do
              for y = 0 to Schedule.Weeks.[x].Days.Length - 1 do
                  for z = 0 to Schedule.Weeks.[x].Days.[y].TimeSlots.Length - 1 do
                      for shift in Schedule.Weeks.[x].Days.[y].TimeSlots.[z].Shifts -> (x, y, z, shift), shift.Strain ]
        |> SMap4.ofList


    // Builds a binary matrix per worker of 3 shifts (as columns) and 7 Days (as Rows) for every employee
    //! Decision
    let shouldWork =
        DecisionBuilder<Shift> "Has to work" {
            for employee in workers do
                for x = 0 to Schedule.Weeks.Length - 1 do
                    for y = 0 to Schedule.Weeks.[x].Days.Length - 1 do
                        for z = 0 to Schedule.Weeks.[x].Days.[y].TimeSlots.Length - 1 do
                            for shift in Schedule.Weeks.[x].Days.[y].TimeSlots.[z].Shifts -> Boolean
        }
        |> SMap5.ofSeq


    let containsAllElements list1 list2 =
        List.forall (fun elem -> List.contains elem list1) list2
    //! Constraints

    // Ensures sufficient, qualified staffing
    let qualifiedConstraints =
        ConstraintBuilder "Ensure qualified personell and enough of workers of in shift" {
            for x = 0 to Schedule.Weeks.Length - 1 do
                for y = 0 to Schedule.Weeks.[x].Days.Length - 1 do
                    for z = 0 to Schedule.Weeks.[x].Days.[y].TimeSlots.Length - 1 do
                        for shift in Schedule.Weeks.[x].Days.[y].TimeSlots.[z].Shifts do
                            for reqPersonnel in shift.RequiredPersonnel ->
                                sum (
                                    shouldWork.[Where(fun employee ->
                                                    containsAllElements
                                                        employee.Occupations
                                                        reqPersonnel.RequiredQualifications),
                                                x,
                                                y,
                                                z,
                                                shift]
                                )
                                >== float (reqPersonnel.Count) * 1.0<Shift>
        }

    // Maximum worktime per week
    let maxHoursConstraints =
        ConstraintBuilder "Maximum Constraint" {
            for employee in workers do
                for week = 0 to Schedule.Weeks.Length - 1 do
                    yield
                        sum (shouldWork.[employee, week, All, All, All] .* shiftLength.[week, All, All, All])
                        <== maxHoursPerWeek
        }

    let minimumHoursConstraint =
        ConstraintBuilder "Minimum hours constraint" {
            for employee in workers do
                for week = 0 to Schedule.Weeks.Length - 1 do
                    yield
                        sum (shouldWork.[employee, week, All, All, All] .* shiftLength.[week, All, All, All])
                        >== minHoursPerWeek
        }

    // No double shift on one day can be worked
    let noDoubleShiftConstraint =
        ConstraintBuilder "No Double Shift Constraint" {
            for employee in workers do
                for x = 0 to Schedule.Weeks.Length - 1 do
                    for y = 0 to Schedule.Weeks.[x].Days.Length - 1 do
                        yield sum (shouldWork.[employee, x, y, All, All]) <== 1.0<Shift>
        }

    let medianEmployeeByWage (list: Employee list) =
        let sortedList = list |> List.sortBy (fun emp -> emp.Wage)
        let len = List.length list

        if len % 2 = 0 then
            sortedList[len / 2 - 1]
        else
            sortedList[len / 2]

    let minimizeStrain =
        sum (shouldWork.[medianEmployeeByWage workers, All, All, All, All] .* strainOfShifts)
        |> Objective.create "Minimize strain on workers" Minimize

    let minimizeCosts =
        [ for employee in workers do
              for x = 0 to Schedule.Weeks.Length - 1 do
                  for y = 0 to Schedule.Weeks.[x].Days.Length - 1 do
                      for z = 0 to Schedule.Weeks.[x].Days.[y].TimeSlots.Length - 1 do
                          for shift in Schedule.Weeks.[x].Days.[y].TimeSlots.[z].Shifts ->
                              shouldWork.[employee, x, y, z, shift]
                              * shiftLength.[x, y, z, shift]
                              * workersWage.[employee] ]
        |> List.sum
        |> Objective.create "Minimize Cost Target" Minimize


    //note Maybe minimize cross product? As it is a matrix?

    let retrieveSolutionValues (result: SolveResult) (stopwatch: Stopwatch) =
        match result with
        | Optimal solution ->
            problemToProtocol problem stopwatch true
            let values = Solution.getValues solution shouldWork |> SMap5.ofMap

            let resultMatrix =
                [ for week = 0 to Schedule.Weeks.Length - 1 do
                      [ for day = 0 to Schedule.Weeks.[week].Days.Length - 1 do
                            [ for timeslot = 0 to Schedule.Weeks.[week].Days.[day].TimeSlots.Length - 1 do
                                  [ for shift in Schedule.Weeks.[week].Days.[day].TimeSlots.[timeslot].Shifts do
                                        [ let x = values.[All, week, day, timeslot, shift]

                                          for employee in workers do
                                              if x.[employee] = 1.0<Shift> then
                                                  yield employee.Name ] ] ] ] ]

            { Status = true
              Result = resultMatrix
              ObjectiveCost = Objective.evaluate solution minimizeCosts
              ObjectiveStrain = Objective.evaluate solution minimizeStrain }
        | _ ->
            problemToProtocol problem stopwatch false

            { Status = false
              Result = [ [ [ [ [ sprintf "[Error]: Model infeasible -> %A" result ] ] ] ] ]
              ObjectiveCost = 0.0<Euro>
              ObjectiveStrain = 0.0<Strain> }


    // Prepare for stats extraction
    let stopwatch = Stopwatch.StartNew()
    //! Solve model
    let solved =
        let options = problem.Options

        let mutable model =
            match (options.ExpenseMinimizing, options.StrainMinimizing) with
            | true, false -> Model.create minimizeCosts
            | false, true -> Model.create minimizeStrain
            | _ -> Model.create minimizeCosts |> Model.addObjective minimizeStrain

        if options.EnsureQualifiedPersonnelConstraint then
            model <- Model.addConstraints qualifiedConstraints model

        if options.NoDoubleShiftConstraint then
            model <- Model.addConstraints noDoubleShiftConstraint model

        if options.MaximumWorkingHoursConstraint then
            model <- Model.addConstraints maxHoursConstraints model

        if options.MinimumWorkingHoursConstraint then
            model <- Model.addConstraints minimumHoursConstraint model

        model |> Solver.solve Settings.basic


    retrieveSolutionValues solved stopwatch


//! Unit test
let testCase () =
    let shifts =
        [ { Name = "Morning Shift"
            RequiredPersonnel =
              [ { Count = 1<Worker / Shift>
                  RequiredQualifications = [ "EMT" ] }
                { Count = 1<Worker / Shift>
                  RequiredQualifications = [ "Doctor" ] } ]
            Length = 8.0<Hour / Shift>
            Strain = 1.2<Strain / Shift> }
          { Name = "Late Shift"
            RequiredPersonnel =
              [ { Count = 1<Worker / Shift>
                  RequiredQualifications = [ "EMT" ] }
                { Count = 1<Worker / Shift>
                  RequiredQualifications = [ "Doctor" ] }
                { Count = 1<Worker / Shift>
                  RequiredQualifications = [ "Nurse" ] } ]
            Length = 8.0<Hour / Shift>
            Strain = 1.0<Strain / Shift> }
          { Name = "Night Shift"
            RequiredPersonnel =
              [ { Count = 1<Worker / Shift>
                  RequiredQualifications = [ "Doctor" ] } ]
            Length = 8.0<Hour / Shift>
            Strain = 1.8<Strain / Shift> } ]

    let workers =
        [ { Name = "Jenna"
            Occupations = [ "EMT" ]
            Wage = 25.0<Euro / Hour> }
          { Name = "Hannah"
            Occupations = [ "Nurse" ]
            Wage = 20.0<Euro / Hour> }
          { Name = "George"
            Occupations = [ "Doctor" ]
            Wage = 30.0<Euro / Hour> }
          { Name = "Freddy"
            Occupations = [ "Doctor" ]
            Wage = 31.0<Euro / Hour> }
          { Name = "Kiley"
            Occupations = [ "Doctor" ]
            Wage = 28.0<Euro / Hour> }
          { Name = "Delta"
            Occupations = [ "EMT" ]
            Wage = 24.0<Euro / Hour> }
          { Name = "Marlee"
            Occupations = [ "Doctor" ]
            Wage = 34.0<Euro / Hour> }
          { Name = "Tucker"
            Occupations = [ "Nurse" ]
            Wage = 18.0<Euro / Hour> }
          { Name = "Lawrence"
            Occupations = [ "EMT" ]
            Wage = 25.0<Euro / Hour> } ]

    let simplexSchedule =
        { Weeks =
            [ { Days =
                  [ { TimeSlots =
                        [ { Shifts = [ shifts.[0] ] }
                          { Shifts = [ shifts.[1] ] }
                          { Shifts = [ shifts.[2] ] } ] }
                    { TimeSlots =
                        [ { Shifts = [ shifts.[0] ] }
                          { Shifts = [ shifts.[1] ] }
                          { Shifts = [ shifts.[2] ] } ] }
                    { TimeSlots =
                        [ { Shifts = [ shifts.[0] ] }
                          { Shifts = [ shifts.[1] ] }
                          { Shifts = [ shifts.[2] ] } ] }
                    { TimeSlots =
                        [ { Shifts = [ shifts.[0] ] }
                          { Shifts = [ shifts.[1] ] }
                          { Shifts = [ shifts.[2] ] }

                          ] }
                    { TimeSlots =
                        [ { Shifts = [ shifts.[0] ] }
                          { Shifts = [ shifts.[1] ] }
                          { Shifts = [ shifts.[2] ] } ] }
                    { TimeSlots =
                        [ { Shifts = [ shifts.[0] ] }
                          { Shifts = [ shifts.[1] ] }
                          { Shifts = [ shifts.[2] ] } ] }
                    { TimeSlots =
                        [ { Shifts = [ shifts.[0] ] }
                          { Shifts = [ shifts.[1] ] }
                          { Shifts = [ shifts.[2] ] } ] }

                    ] } ] }

    { Workers = workers
      Schedule = simplexSchedule
      MaxHoursPerWeek = 50.0<Hour>
      MinHoursPerWeek = 0.0<Hour>
      Options =
        { ExpenseMinimizing = true
          StrainMinimizing = true
          MaximumWorkingHoursConstraint = true
          EnsureQualifiedPersonnelConstraint = true
          NoDoubleShiftConstraint = true
          MinimumWorkingHoursConstraint = false } }
