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
let biggestCommon(inputList: List<int>): int =
  let rec recur(list: List<int>, num: int): int =
    let filtered = list |> List.filter (fun x -> x >= num)
    // printfn "filtered %A" filtered
    let len = filtered.Length
    // printfn "biggestCommon %d %d %d" len list.Length num
    if len = list.Length then recur(list, num+1)
    else num-1
  recur(inputList, 0)

let findSquareInList(input: List<int>): Square =
  let subIndex = input.Length
  let rec recur(list: List<int>, index: int, acc: Square): Square =
    match list with
    | [] -> acc
    | head::tail ->
                    let biggest = biggestCommon list
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
              if square.SideLength > acc.SideLength then recur(tail, index+1, square)
              else recur(tail, index+1, acc)
  recur(input, 0, Square(0,0))

let readInputData =
  let dataList = File.ReadLines("./input.txt")
              |> Seq.toList
              |> List.map int
  findSquare(dataList.[..1000])

[<EntryPoint>]
let main argv =
    printfn "%A" readInputData
    0 // return an integer exit code

