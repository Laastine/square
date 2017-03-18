module square

open System
open System.IO

[<StructuredFormatDisplay("{AsString}")>]
type Square =
  struct
    val Index: int
    val Common: int
    val Biggest: int

    new (index, common, biggest) = {Index = index; Common = common; Biggest = biggest;}
    override s.ToString() = sprintf "index={%d}, common={%d}, biggest={%d}" s.Index s.Common s.Biggest
    member s.AsString = s.ToString()
end

let biggestCommon(inputList: List<int>): int =
  let biggerThan(a: int, num: int): bool = a >= num
  let rec recur(list: List<int>, num: int): int =
    let len = list |> List.filter (fun x -> biggerThan(x, num)) |> List.length
    if len = list.Length then recur(list, num+1) else num-1
  recur(inputList, 1)

let removeLastElement(input: List<'T>): List<'T> =
  input |> List.rev |> List.tail |> List.rev

let findSquare(input: List<int>): Square =
  let rec recur(list: List<int>, index: int, acc: Square): Square =
    match list with
    | [] -> acc
    | head::tail ->
                    let biggest = biggestCommon list
                    printfn "biggest %A" biggest
                    let newTail = removeLastElement list
                    let squareSideLength = if biggest > tail.Length then tail.Length else biggest
                    printfn "newTail %A" newTail
                    if squareSideLength > newTail.Length then acc
                    else if acc.Common < biggest then recur(newTail, index+1, Square(index, squareSideLength, biggest))
                    else recur(newTail, index+1, acc)
  recur(input, 0, Square(0,0,0))

let readInputData =
  let data = File.ReadLines("./input2.txt")
              |> Seq.toList
              |> List.map int
  findSquare data

[<EntryPoint>]
let main argv =
    printfn "%A" readInputData
    0 // return an integer exit code


// 40
// 23
// 220
// 20
// 2
// 24
// 45
// 65
// 432
// 34