module ModularisedScheduling

open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure

[<Measure>] type Euro
[<Measure>] type Hour
[<Measure>] type Strain
[<Measure>] type Worker
[<Measure>] type Shift


// Challenge: Create a model that is able to create schedule that minimizes 
// costs and strain on workers while respecting these constraints:
(*
    - No worker may work over 40 hours a week
    - No worker may work 2 shifts in one day
    - Each shift requires a specific amount of workers of a certain occupation
*)
// As well as minimizes possible code duplications and maximizes extensibility and modularity


//! Domain Model
type ShiftInfo = {
    Name: string
    RequiredPersonal: (int<Worker/Shift> * string) list
    Length: float<Hour/Shift>
    Strain: float<Strain>
}


type Employee = {
    Name:string
    Occupation:string
    Wage:float<Euro/Hour>
}


let version = "beta-1.0.0"

let constructProblem (shifts:ShiftInfo list) (workers: Employee list) (workWeeksAmount:int) (maxHoursPerWeek:float<Hour>) =

    let workWeeks = [1 .. workWeeksAmount]

    let workersWage =
        [for record in workers -> record, record.Wage] |> SMap.ofList

    //! Shift information
    let workdays = [1..7]


    // Here are the shifts helpers defined
    let shiftLength = 
        [for shift in shifts -> shift, shift.Length] |> SMap.ofList


    let strainOfShifts =
        [for shift in shifts -> shift, shift.Strain] |> SMap.ofList


    // Builds a binary matrix per worker of 3 shifts (as columns) and 7 days (as Rows) for every employee
    //! Decision
    let shouldWork =
        DecisionBuilder<Shift> "Has to work" {
            for employee in workers do
                for week in workWeeks do
                    for day in workdays do
                        for shift in shifts ->
                            Boolean
        } |> SMap4.ofSeq
        
    //! Constraints
    (*
        We need more or an equal amount of workers of the matching profession to be working per shift requirements:
        - shouldWork.[Where(employee = reqProfession), day, shift] >== Count<Worker/Shift>
        
        Each worker can only work a certain amount of hours
        - shouldWork.[employee, All, All] <== x<Hour>
    
        No worker can enter 2 shifts per day
        - shouldWork.[employee, day, All] <== 1.0<Shift>
    *)
    
    // Ensures sufficient, qualified staffing
    let qualifiedConstraints =
        ConstraintBuilder "Is qualified and enough workers of in shift" {
            for week in workWeeks do
                for day in workdays do
                    for shift in shifts do
                        for (reqWorkers, qualification) in shift.RequiredPersonal ->
                            sum(shouldWork.[Where (fun employee -> employee.Occupation = qualification), week, day, shift]) >== float(reqWorkers) * 1.0<Shift>
        }

    // Maximum worktime per week
    let maxHoursConstraints =
        ConstraintBuilder "Maximum Constraint" {
            
            for employee : Employee in workers do
                for week in workWeeks ->
                    sum (shouldWork.[employee,week,All,All] .* shiftLength) <== maxHoursPerWeek
        }
    
    // No double shift on one day can be worked
    let noDoubleShiftConstraint =
        ConstraintBuilder "No Double Shift Constraint" {
            for employee in workers do
                for week in workWeeks do
                    for day in workdays ->
                        sum(shouldWork.[employee,week,day,All]) <== 1.0<Shift>
    }

    //! Objectives
    let minimizeStrain =
        [
            for employee in workers do
                for week in workWeeks do
                    for day in workdays do
                        for shift in shifts ->
                            sum (shouldWork.[employee,week,day,All] .* strainOfShifts)
        ]
        |> List.sum
        |> Objective.create "Minimize strain on workers" Minimize

    let minimizeCosts =
        [
            for employee in workers do
                for week in workWeeks do
                    for day in workdays do
                        for shift in shifts ->
                            shouldWork.[employee,week,day,shift] * shiftLength.[shift] * workersWage.[employee]
        ]
        |> List.sum
        |> Objective.create "Minimize Cost Target" Minimize

    //todo Implement a way to minimize shift switches
    //note Maybe minimize cross product? As it is a matrix?

    let printDash() =
        for x in [0..100] do
            printf "-"
        printf "\n"

    //! Printing method
    let printResult result =
        match result with
        | Optimal solution ->
            printfn "Minimal personal costs:      %.2f" (Objective.evaluate solution minimizeCosts)
            printfn "Minimal strain on employees: %.2f" (Objective.evaluate solution minimizeStrain)
            let values = Solution.getValues solution shouldWork |> SMap4.ofMap
            for employee in workers do
                let solutionmatrix =
                    [for week in workWeeks do [for day in workdays do [for shift in shifts -> values.[employee,week,day,shift]]]]
                printfn "%s" (employee.Name)
                for shift in shifts do
                    printf "(%s) " (shift.Name)
                printf "\n"
                for week in workWeeks do
                    for day in workdays do 
                        printf "%A\n" (solutionmatrix[week - 1][day - 1])

            //! Print working plan by Name

            let formattedTable =
                [
                    for week in workWeeks do 
                    [
                        for day in workdays do
                        [
                            for shift in shifts do
                            [
                                let x = values.[All,week,day, shift]
                                for employee in workers do
                                    if x.[employee] = 1.0<Shift> then yield employee.Name
                            ]
                        ]
                    ]
                ]

            printfn "Schedule: "
            for shift in shifts do
                    printf "(%s) " (shift.Name)
            printf "\n"

            printDash()

            for week in workWeeks do
                for day in workdays do
                    printfn "%d | %A" (day) (formattedTable[week - 1][day - 1])
                printDash()
            

        | _ -> printfn $"Unable to solve. Error: %A{result}. This might be because of a problem in the domain model or a conflicting constraint like the 'Max working hours'"

    //! Solve model
    minimizeCosts
    |> Model.create
    |> Model.addObjective minimizeStrain
    |> Model.addConstraints qualifiedConstraints
    |> Model.addConstraints noDoubleShiftConstraint
    |> Model.addConstraints maxHoursConstraints
    |> Solver.solve Settings.basic
    |> printResult


//! Unit test
//let solve_test() =
//    let shifts = 
//        [
//            {Name="Morning Shift"; RequiredPersonal=[(1<Worker/Shift>, "EMT"); (1<Worker/Shift>,"Doctor")];                             Length=8.0<Hour/Shift>;    Strain=1.2<Strain>}
//            {Name="Late Shift";    RequiredPersonal=[(1<Worker/Shift>, "EMT"); (1<Worker/Shift>,"Doctor"); (1<Worker/Shift>, "Nurse")]; Length=8.0<Hour/Shift>;    Strain=1.0<Strain>}
//            {Name="Night Shift";   RequiredPersonal=[(1<Worker/Shift>, "Doctor")];                                                      Length=8.0<Hour/Shift>;    Strain=1.8<Strain>}
//        ]

//    let workers = 
//        [
//            {Name="Jenna";    Occupation = "EMT";     Wage=25.0<Euro/Hour>}
//            {Name="Hannah";   Occupation = "Nurse";   Wage=20.0<Euro/Hour>}
//            {Name="George";   Occupation = "Doctor";  Wage=30.0<Euro/Hour>}
//            {Name="Freddy";   Occupation = "Doctor";  Wage=31.0<Euro/Hour>}
//            {Name="Kiley";    Occupation = "Doctor";  Wage=28.0<Euro/Hour>}
//            {Name="Delta";    Occupation = "EMT";     Wage=24.0<Euro/Hour>}
//            {Name="Marlee";   Occupation = "Doctor";  Wage=34.0<Euro/Hour>}
//            {Name="Lawrence"; Occupation = "EMT";     Wage=25.0<Euro/Hour>}
//            {Name="Tucker";   Occupation = "Nurse";   Wage=18.0<Euro/Hour>}
//        ]

//    constructProblem shifts workers 4 50.0<Hour>