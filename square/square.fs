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
  if windowMin <= newWindow.Length then newWindow.[newWindow.Length-windowMin..]
  else newWindow

let slidingWindow(input: List<int>): Square =
  let rec recur(list: List<int>, index: int, slidingWindow: List<int>, acc: Square): Square =
    match list with
    | [] -> acc
    | head::tail ->
                  let newWindow = adjustIfSlidingWindowTooLong(slidingWindow, head)
                  if newWindow.Length > acc.SideLength then recur(tail, index+1, newWindow, Square(index, newWindow.Length))
                  else recur(tail, index+1, newWindow, acc)
  recur(input, 0, [], Square(0,0))

let readInputData =
  File.ReadLines("./input.txt")
    |> Seq.toList
    |> List.map int
    |> slidingWindow

let testFunction =
  adjustIfSlidingWindowTooLong([6;3;], 3) = [6;3;3;] &&
  adjustIfSlidingWindowTooLong([3;2;], 1) = [1;] &&
  adjustIfSlidingWindowTooLong([], 1) = [1;] &&
  adjustIfSlidingWindowTooLong([8;5;9;4;], 10) = [5;9;4;10] &&
  adjustIfSlidingWindowTooLong([51;123;5;124;124;8;], 8) = [5; 124; 124; 8; 8] &&
  adjustIfSlidingWindowTooLong([51;123;5;124;124;], 8) = [123;5;124;124;8;] &&
  adjustIfSlidingWindowTooLong([51;123;5;124;124;], 3) = [124;124;3;]

[<EntryPoint>]
let main argv =
    printfn "%A" readInputData
    0 // return an integer exit code

