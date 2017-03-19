module square

open System
open System.IO

[<StructuredFormatDisplay("{AsString}")>]
type Square =
  struct
    val Index: int
    val SideLength: int

    new (index, sideLength) = {Index = index; SideLength = sideLength;}
    override s.ToString() = sprintf "index={%d}, sideLength={%d}" s.Index s.SideLength
    member s.AsString = s.ToString()
end


let adjustIfSlidingWindowTooLong(window: List<int>, head: int): List<int> =
  let newWindow = if window.IsEmpty then [head] else List.concat [window; [head]]
  let windowMin = newWindow |> List.min
  if windowMin <= newWindow.Length then
    printfn "1 splitIndex %d" (newWindow.Length-windowMin)
    newWindow.[newWindow.Length-windowMin..]
  else newWindow

let adjustIfNewHeadElementTooSmallToWindow(window: List<int>, head: int): List<int> =
  let newWindow = if window.IsEmpty then [head] else List.concat [window; [head]]
  if window.IsEmpty then [head]
  else if (window |> List.min) > head then
    let isPresent = List.exists (fun x -> x > head) window
    let splitIndex =
      if isPresent then
        (window |> List.findIndex (fun x -> x > head)) + 1
      else 0
    printfn "2 splitIndex %d" (splitIndex)
    newWindow.[window.Length - splitIndex..]
  else newWindow

let createNewWindow(window: List<int>, head: int, acc: Square): List<int> =
  let windowMin = if window.IsEmpty then 0 else window |> List.min
  printfn "createNewWindow %A %A" head window
  adjustIfSlidingWindowTooLong(window, head)

let slidingWindow(input: List<int>): Square =
  let rec recur(list: List<int>, index: int, slidingWindow: List<int>, acc: Square): Square =
    match list with
    | [] -> acc
    | head::tail ->
                  // printfn "slidingWindow %d %A" head slidingWindow
                  let newWindow = createNewWindow(slidingWindow, head, acc)
                  if newWindow.Length > acc.SideLength then recur(tail, index+1, newWindow, Square(index, newWindow.Length))
                  else recur(tail, index+1, newWindow, acc)
  recur(input, 0, [], Square(0,0))

let readInputData =
  let dataList = File.ReadLines("./input2.txt")
              |> Seq.toList
              |> List.map int
  slidingWindow(dataList)

let testFunction =
  adjustIfSlidingWindowTooLong([6;3;], 3) = [6;3;3;] && adjustIfSlidingWindowTooLong([3;2;], 1) = [1;] && adjustIfSlidingWindowTooLong([], 1) = [1;] &&
  // adjustIfNewHeadElementTooSmallToWindow([6;3;], 3) = [6;3;3;] && adjustIfNewHeadElementTooSmallToWindow([3;2;], 1) = [1;] && adjustIfNewHeadElementTooSmallToWindow([], 1) = [1;] &&
  adjustIfSlidingWindowTooLong([8;5;9;4;], 10) = [5;9;4;10] &&
  adjustIfSlidingWindowTooLong([51;123;5;124;124;8;], 8) = [5; 124; 124; 8; 8] &&
  createNewWindow([51;123;5;124;124;], 8, Square(0,0)) = [123;5;124;124;8;] &&
  createNewWindow([51;123;5;124;124;], 3, Square(0,0)) = [124;124;3;]


[<EntryPoint>]
let main argv =
    printfn "%A" readInputData
    0 // return an integer exit code

