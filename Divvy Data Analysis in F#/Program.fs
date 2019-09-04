//
// F# program to analyze Divvy daily ride data.
//
// Raja Patel
// U. of Illinois, Chicago
// CS 341, Spring 2019
// Project #04
//

#light

// module project04

//
// ParseLine and ParseInput
//
// Given a sequence of strings representing Divvy data, 
// parses the strings and returns a list of lists.  Each
// sub-list denotes one bike ride.  Example:
//
//   [ [176;74;1252;21;595;1986;1]; ... ]
//
// The values are station id (from), station id (to), bike
// id, starting hour (0..23), trip duration (secs), birth
// year (0=>not specified), and gender (0=>not specified, 
// 1=>identifies as male, 2=>identifies as female).
//

let ParseLine (line:string) = 
 let tokens = line.Split(',')
 let ints = Array.map System.Int32.Parse tokens
 Array.toList ints

let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides
  

let listResult L = List.countBy (fun elem ->
 if (elem = 1) then 0 else 1) L

let parseList L = List.map (fun [_; _; _; _; _; _; s] -> s) L

//let parseList2 L a = Seq.filter ((=) a) >> Seq.length

//Counts the number of Males
let isOne =
    fun c -> ['1'] |> List.contains c
let countMen =
    String.filter isOne
    >> String.length

//Counts the number of Females
let isTwo =
    fun c -> ['2'] |> List.contains c
let countWomen =
    String.filter isTwo
    >> String.length

//Implode function from HW4 modified to fit Int
let implode (L:int list) = 
    let sb = System.Text.StringBuilder()
    L|> List.iter (fun c-> ignore (sb.Append (c:int)))
    sb.ToString()

//counts percentage
let percentage numerator denominator =
 match denominator with
 | 0.0 -> 0.0
 |  _  -> (numerator/denominator) * 100.00


let ageCount numerator denominator = 
    match denominator with
    | 0.0 -> 0.0
    |  _  -> (float(2019) - (float(numerator)/float(denominator)))

//printstar function: the histogram
let rec printstars n = 
 match n with
 | 0 -> ()
 | 1 -> printf "*"
 | _ -> printf "*"
        printstars (n-1)


let parseDuration L = List.map (fun [_; _; _; _; s; _; _] -> s) L

let HowManySatisfy pred = Seq.filter pred >> Seq.length 
let sum30 L = L |> HowManySatisfy (fun n -> n<=1800) 
let sum60 L = L |> HowManySatisfy (fun n -> n>1800 && n<=3600)
let sum120 L = L |> HowManySatisfy (fun n -> n>3600 && n<=7200)
let sum2hr L = L |> HowManySatisfy (fun n -> n>7200)
 

let parseYear L = List.map (fun [_; _; _; _; _; s; _] -> s ) L  

let listAge L = List.countBy (fun elem ->
 if (elem > 0) then elem else 1) L

let listAgeSum L = List.sumBy (fun elem ->
 if (elem > 0) then elem else 0) L
//let sumAge L = List.sumBy (fun n -> n>0 )

let ageN L = L |> HowManySatisfy (fun n -> n>0)

let parserideTime L = List.map (fun [_; _; _; s ; _; _; _] -> s ) L

let listPrint L = Seq.iter (fun x -> printf "%A " x)



let hour0 L k = L |> HowManySatisfy (fun n -> n = k)

[<EntryPoint>]
let main argv =
  //////////////////////////////////////////////////////////////////////

  // input file name, then input divvy ride data and build
  // a list of lists:
  //
  printf "filename> "
  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents

  //printfn "%A" ridedata
  let N = List.length ridedata
  printfn ""
  printfn "# of riders: %A" N
  printfn ""

  //Parses the list Male and Female Data
  let K = parseList ridedata
  let K2 = implode K 

  //Prints the info: Males
  let Men = countMen K2
  printf "%% of riders identifying as male: %A " Men
  let percentMen = percentage (float(Men)) (float(N))
  printf "(" 
  printf "%A" percentMen
  printf "%%)"
  printfn ""

  //Prints the info: Females
  let Women = countWomen K2
  printf "%% of riders identifying as female: %A " Women
  let percentWomen = percentage (float(Women)) (float(N))
  printf "(" 
  printf "%A" percentWomen
  printf "%%)"
  printfn ""

  //////////////////////////////////////////////////////////////////////

  //Prints average Age
  let Y = parseYear ridedata
  let ageSum = listAgeSum Y
  let ageN = ageN Y
  let age = ageCount ageSum (float(ageN))
  printfn ""
  printfn "Average age: %A" age

  //////////////////////////////////////////////////////////////////////

  //Prints the info: Ride Durations
  let P = parseDuration ridedata

  printfn ""
  printfn "** Ride Durations:"

  let C30 = sum30 P
  let percent30 = percentage (float(C30)) (float(N))
  printf " 0..30 mins: "
  printf "%A " C30
  printf "(" 
  printf "%A" percent30
  printf "%%)"
  printfn ""

  let C60 = sum60 P
  let percent60 = percentage (float(C60)) (float(N))
  printf " 30..60 mins: "
  printf "%A " C60
  printf "(" 
  printf "%A" percent60
  printf "%%)"
  printfn ""

  let C120 = sum120 P
  let percent120 = percentage (float(C120)) (float(N))
  printf " 60..120 mins: "
  printf "%A " C120
  printf "(" 
  printf "%A" percent120
  printf "%%)"
  printfn ""

  let C2hr = sum2hr P
  let percent2hr = percentage (float(C2hr)) (float(N))
  printf " > 2 hours: "
  printf "%A " C2hr
  printf "(" 
  printf "%A" percent2hr
  printf "%%)"
  printfn ""


  let R = parserideTime ridedata

  //printf "%A" R

 

  let zero = hour0 R 0
  let one = hour0 R 1
  let two = hour0 R 2
  let three = hour0 R 3
  let four = hour0 R 4
  let five = hour0 R 5
  let six = hour0 R 6
  let seven = hour0 R 7
  let eight = hour0 R 8
  let nine = hour0 R 9
  let ten = hour0 R 10
  let eleven = hour0 R 11
  let twelve = hour0 R 12
  let thirteen = hour0 R 13
  let fourteen = hour0 R 14
  let fifteen = hour0 R 15
  let sixteen = hour0 R 16
  let seventeen = hour0 R 17
  let eighteen = hour0 R 18
  let nineteen = hour0 R 19
  let twenty = hour0 R 20
  let twenty1 = hour0 R 21
  let twenty2 = hour0 R 22
  let twenty3 = hour0 R 23

  printfn " "
  printf "** Ride Start Time Histogram: "
  printfn " "

  printf " 0: "
  printstars (zero/10)
  printfn "%A " zero

  printf " 1: "
  printstars (one/10)
  printfn "%A " one

  printf " 2: "
  printstars (two/10)
  printfn "%A " two

  printf " 3: "
  printstars (three/10)
  printfn "%A " three

  printf " 4: "
  printstars (four/10)
  printfn "%A " four

  printf " 5: "
  printstars (five/10)
  printfn "%A " five

  printf " 6: "
  printstars (six/10)
  printfn "%A " six

  printf " 7: "
  printstars (seven/10)
  printfn "%A " seven

  printf " 8: "
  printstars (eight/10)
  printfn "%A " eight


  printf " 9: "
  printstars (nine/10)
  printfn "%A " nine


  printf " 10: "
  printstars (ten/10)
  printfn "%A " ten

  printf " 11: "
  printstars (eleven/10)
  printfn "%A " eleven

  printf " 12: "
  printstars (twelve/10)
  printfn "%A " twelve

  printf " 13: "
  printstars (thirteen/10)
  printfn "%A " thirteen

  printf " 14: "
  printstars (fourteen/10)
  printfn "%A " fourteen

  printf " 15: "
  printstars (fifteen/10)
  printfn "%A " fifteen

  printf " 16: "
  printstars (sixteen/10)
  printfn "%A " sixteen

  printf " 17: "
  printstars (seventeen/10)
  printfn "%A " seventeen

  printf " 18: "
  printstars (eighteen/10)
  printfn "%A " eighteen

  printf " 19: "
  printstars (nineteen/10)
  printfn "%A " nineteen

  printf " 20: "
  printstars (twenty/10)
  printfn "%A " twenty


  printf " 21: "
  printstars (twenty1/10)
  printfn "%A " twenty1


  printf " 22: "
  printstars (twenty2/10)
  printfn "%A " twenty2


  printf " 23: "
  printstars (twenty3/10)
  printfn "%A " twenty3








 

  0 
