// Joseph Madden

open TreeSearch
open System

type State = char []

let goalState = [|  'L'; 'L'; 'L'; ' '; 'R'; 'R'; 'R' |]

let startState0 = [|  'L'; 'L'; 'L'; 'R'; ' '; 'R'; 'R' |]
let startState1 = [|  'L';  'R'; ' '; 'L';  'L'; 'R';'R' |]
let startState2 = [|  'L';  'R'; 'R';   'L';  'L'; 'R'; ' ' |] 
let startState3 = [| 'R'; 'R'; ' ';  'L'; 'L'; 'R'; 'L' |] 
let startState4 = [| 'R'; 'R'; 'R'; ' '; 'L'; 'L'; 'L' |]

let startState = startState2

let goalTest s = s = goalState

let copyState (s:State) =
    let newState:State = [| '.'; '.'; '.'; '.'; '.'; '.'; '.' |]
    for i in 0..6 do
        newState.[i] <- s.[i]
    newState

let isGoalState (s:State) =
    let mutable ans = true
    for i in 0..6 do
        if not (s.[i] = goalState.[i]) then
            ans <- false
    ans
    
let findBlank (st:State) =
    let mutable spot = 0
    for idx in 0..6 do
        if st.[idx] = ' ' then
            spot <- idx
    spot
       
// actions
let slideLeft (st:State) =
    let s = copyState st
    let blank = findBlank s
    if blank >= 1 then
        let moved = s.[blank-1]
        s.[blank] <- moved
        s.[blank-1] <- ' '
    s
    
let hopLeft (st:State) = 
    let s = copyState st
    let blank = findBlank s
    if blank >= 2 then
        let moved = s.[blank-2]
        s.[blank] <- moved
        s.[blank-2] <- ' ' 
    s

let hop2Left (st:State) =   
    let s = copyState st
    let blank = findBlank s
    if blank >= 3 then
        let moved = s.[blank-3]
        s.[blank] <- moved
        s.[blank-3] <- ' ' 
    s

let slideRight (st:State) = 
    let s = copyState st
    let blank = findBlank s
    if blank <= 5 then
        let moved = s.[blank+1]
        s.[blank] <- moved 
        s.[blank+1] <- ' '
    s

let hopRight (st:State) = 
    let s = copyState st
    let blank = findBlank s
    if blank <= 4 then
        let moved = s.[blank + 2]
        s.[blank] <- moved
        s.[blank + 2] <- ' ' 
    s

let hop2Right (st:State) = 
    let s = copyState st
    let blank = findBlank s
    if blank <= 3 then
        let moved = s.[blank+3]
        s.[blank] <- moved
        s.[blank+3] <- ' '
    s

let actions = [| slideLeft; hopLeft; hop2Left; slideRight; hopRight; hop2Right; |]
let names = [| "slideLeft"; "hopLeft"; "hop2Left"; "slideRight"; "hopRight"; "hop2Right"; |] 
let costs = [| 2.0; 3.0; 4.0; 2.0; 3.0; 4.0;|]

let heuristic (st:State) =
    let mutable tilesWrong = 0
    let s = copyState st
    for i in 0..6 do
       if not (s.[i] = goalState.[i]) then
            tilesWrong <- tilesWrong + 1
    (double tilesWrong)

let problem = { Start = startState;
                IsGoal = goalTest;
                Actions = actions;
                Names = names;
                Costs = costs; 
                Heuristic = heuristic;}

let startTime = DateTime.Now
let (satisfied,g) = ucs problem 
let finishTime = DateTime.Now
let elapsed = finishTime - startTime

printfn "%A" g
printfn "with %d steps" g.Depth
printfn "%d nodes were expanded" nodesExpanded
printfn "Took time: %A" elapsed

System.Console.ReadLine() |> ignore

