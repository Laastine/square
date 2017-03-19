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

let removeLastElement(input: List<'T>): List<'T> = input |> List.rev |> List.tail |> List.rev

let findSquareInList(input: List<int>): Square =
  let subIndex = input.Length
  let rec recur(list: List<int>, index: int, acc: Square): Square =
    match list with
    | [] -> acc
    | head::tail ->
                    let biggest = list |> List.min
                    // printfn "biggest %d %d" biggest tail.Length
                    let sideLength = if biggest <= tail.Length then biggest else list.Length
                    let newTail = removeLastElement list
                    // printfn "SideLength %A list %A acc %A" sideLength list acc
                    if acc.SideLength > newTail.Length then acc
                    else if acc.SideLength < biggest && acc.SideLength < sideLength then recur(newTail, index, Square(index, sideLength))
                    else recur(newTail, index, acc)
  recur(input, subIndex, Square(0, 0))

let findSquare(input: List<int>): Square =
  let rec recur(list: List<int>, index: int, acc: Square): Square =
    match list with
    | [] -> acc
    | head::tail ->
              let square = findSquareInList(head::tail)
              let windowMin = head::tail |> List.min
              if square.SideLength > acc.SideLength && square.SideLength < windowMin then recur(tail, index+1, square)
              else recur(tail, index+1, acc)
  recur(input, 0, Square(0,0))


let createNewWindow(window: List<int>, head: int, acc: Square): List<int> =
  let windowMin = if window.IsEmpty then 0 else window |> List.min
  printfn "createNewWindow %A %A, min %A" head window windowMin
  if window.IsEmpty then [head]
  else if windowMin < window.Length then
      // printfn "1 Splitting %A %A" window head
      let isPresent = List.exists (fun x -> x < window.Length) window
      let splitIndex =  if isPresent then List.findIndex (fun x -> x < window.Length) window
                        else window.Length
      // printfn "1 splitIndex %d" (splitIndex+1)
      List.concat [window.[splitIndex+1..window.Length-1]; [head]]
  else if windowMin < head then
      let isPresent = List.exists (fun x -> x < head) window
      let splitIndex =  if isPresent then List.findIndex (fun x -> x < head) window
                        else window.Length
      // printfn "2 splitIndex %d" splitIndex
      List.concat [window.[splitIndex..window.Length-1]; [head]]
  else if acc.SideLength > head then [head]
  else List.concat [window; [head]]

let slidingWindow(input: List<int>): Square =
  let rec recur(list: List<int>, index: int, slidingWindow: List<int>, acc: Square): Square =
    match list with
    | [] -> acc
    | head::tail ->
                  // printfn "slidingWindow %d %A" head slidingWindow
                  let newWindow = createNewWindow(slidingWindow, head, acc)
                  if List.length newWindow > acc.SideLength then recur(tail, index+1, newWindow, Square(index, newWindow.Length))
                  else recur(tail, index+1, newWindow, acc)
  recur(input, 0, [], Square(0,0))

let readInputData =
  let dataList = File.ReadLines("./input2.txt")
              |> Seq.toList
              |> List.map int
  slidingWindow(dataList)

[<EntryPoint>]
let main argv =
    printfn "%A" readInputData
    0 // return an integer exit code

