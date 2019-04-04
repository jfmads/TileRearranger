// Joseph Madden
module TreeSearch

type Problem<'State> = 
    { 
      Start : 'State; 
      IsGoal : 'State -> bool; 
      Heuristic : 'State -> double;
      Actions : array<'State -> 'State>; 
      Names : array<string>
      Costs : array<double>
    }

type Node<'State> =
    { 
      State : 'State;
      Parent : 'State;
      Action : int;
      Depth : int;
      Cost : double
      Value : double
    }

let mutable nodesExpanded = 0

let expand problem node =
    nodesExpanded <- nodesExpanded + 1
    [ for i = 0 to problem.Actions.Length-1 do 
        let action = problem.Actions.[i]
        yield { State = action node.State;
                Parent = node.State;
                Action = i;
                Depth = node.Depth + 1
                Cost = node.Cost + problem.Costs.[i];
                Value = 0.0 }
    ]

let treeSearch problem combiner =
    //nodesExpanded <- 0
    let start = { State = problem.Start;
                  Parent = problem.Start;
                  Action = -1;  // not a valid array index - no action
                  Depth = 0;
                  Cost = 0.0;
                  Value = 0.0 }

    let mutable frontier = [start]
    let mutable goalSatisfied = false
    let mutable currentNode = start

    while (not (List.isEmpty frontier)) && (not goalSatisfied) do
        currentNode <- frontier.Head
       // printf "%A" currentNode.State
        if problem.IsGoal currentNode.State then
            goalSatisfied <- true
        else
            frontier <- combiner (expand problem frontier.Head) frontier.Tail
    (goalSatisfied, currentNode)
    
let dfs problem = treeSearch problem List.append

(* let bfs problem = 
    let prepend children old = old @ children
    treeSearch problem prepend *)

let bfs problem = treeSearch problem (fun l1 l2 -> l2 @ l1)

let dls problem limit =
    let dlsCombine l1 l2 = (l1 @ l2) |> List.filter (fun n -> n.Depth <= limit)
    treeSearch problem dlsCombine

let ids problem = 
    let mutable limit = 1
    let mutable result = dls problem limit
    while fst result |> not do
        limit <- limit + 1
        result <- dls problem limit
    result

let ucs problem =
   let ucCombine children old = (children @ old) |> List.sortBy (fun n -> n.Cost)
   treeSearch problem ucCombine

let gbfs problem =
    let gbfCombine children old =
        let newChildren = [for n in children -> { n with Value = problem.Heuristic n.State }]
        (newChildren @ old) |> List.sortBy (fun n -> n.Value )
    treeSearch problem gbfCombine

let asts problem =
    let astCombine children old =
        let newChildren = [for n in children -> { n with Value = n.Cost + problem.Heuristic n.State }]
        (newChildren @ old) |> List.sortBy (fun n -> n.Value )
    treeSearch problem astCombine
    
let fls problem limit = 
    let flsCombine l1 l2 = (l1 @ l2) |> List.filter (fun n -> n.Cost + problem.Heuristic n.State <= limit)
    treeSearch problem flsCombine
    
let idas problem =
    let mutable limit = 1.0
    let mutable result = fls problem limit
    while fst result |> not do
        limit <- limit + 1.0
        result <- fls problem limit
    result
