// (null: string) |> Option.ofObj // evaluates to None
//  "not a null string" |> Option.ofObj // evaluates to (Some "not a null string")

// Expressions
// let fnn x = if x > 0 then "true"

// Lists
[1;2;3;4;5]
[1..10]
[1..10..100]
[for i in 1..10 -> i * i]

// Sequences a.k.a enumerables
seq { 0..10..100 }
seq { for i in 1 .. 10 -> i * i }
seq { for i in 1 .. 10 do if i < 5 then i }
Seq.empty
Seq.singleton 10
Seq.initInfinite (fun x -> x + 1)
(+) 5 |> Seq.initInfinite
Seq.init 5 (fun n -> n * 10)

let multiplicationTable =
    seq {
        for i in 1..9 do
            for j in 1..9 ->
                (i, j, i*j)
    }

multiplicationTable |> Seq.where (fun (x,_,_) -> x = 3)
multiplicationTable |> Seq.where (fun (x,_,_) -> x = 3) |> List.ofSeq

let isPrime n =
  let sqrt' = (float >> sqrt >> int) n // square root of integer
  [ 2 .. sqrt' ] // all numbers from 2 to sqrt'
  |> List.forall (fun x -> n % x <> 0) // no divisors

seq { for n in 1..100 do if isPrime n then n }

// Back to lists
List.map (fun x -> x + 1) [1..10]
List.map2 (fun x y -> x + y) [1..10] [11..20]

[1..10] |> List.sum
[1..10] |> List.filter (fun x -> x = 6) // where
[10..-1..1] |> List.sort

List.zip [1; 2] ["one"; "two"]
List.unzip [(1, "one"); (2, "two")]

[1..10] |> List.partition (fun x -> x % 2 = 0)

1::[] // List.Cons (1,[])
[1;2] @ [3;4]

List.transpose [
  [1;2;3];
  [4;5;6];
  [7;8;9]
]

// Reduce
let max x y = if x > y then x else y
List.reduce max [1;3;9;3;5]
List.reduce max ["a";"b";"c"]

// Folds
//
List.fold (fun acc x -> acc + x) 0 [1..10]
List.fold (+) 0 [1..10]
List.fold (*) 1 [1..10]
List.fold (fun acc x -> x::acc) [] [1..6]
List.foldBack (fun x acc -> x::acc) [1..6] []

let testadd x y = 
  (+) x y = x + y

testadd 1 3

let rec fold (f: 'a -> 'b -> 'a) (initial: 'a) (li: 'b list) : 'a =
  match li with
    | [] -> initial
    | x::xs -> printfn $"head = {x}; tail = {xs}; acc = {initial}"; fold f (f initial x) xs

// fold f init [1; 2; 3] = (f (f (f init 1) 2) 3)
// fold (+) 0 [1; 2; 3] = ((+) ((+) ((+) 0 1) 2) 3)

fold (fun acc x -> x::acc) [] [1;2;3;4;5;6] // reverse

let rec foldBack (f: 'a -> 'b -> 'b) (li: 'a list) (initial: 'b) : 'b =
  printf $"acc = {initial}; "
  match li with
    | [] -> initial
    | x::xs -> printfn $"head = {x}; tail = {xs}; acc = {initial}"; f x (foldBack f xs initial)

// List.foldBack f [1; 2; 3] init = f 1 (f 2 (f 3 init)).
foldBack (fun x acc -> x::acc) [1;2;3;4;5;6] []

// Note that if 'a = 'b and f is commutative (that is, f a b = f b a for any a, b) 
// then fold and foldBack perform the same operation. 
// Below is an example with a list of length 3, but this really is without loss of generality.
let f a b = a + b
fold f 0 [1;2;3]
foldBack f [1;2;3] 0
// math prove
let theInput = [1;2;3]
fold f 0 theInput = foldBack f theInput 0

([1..5], 0) ||> List.foldBack (fun v acc -> acc + v * v)
(0, [1..5]) ||> List.fold (fun acc v -> acc + v * v)

// More examples of fold
let fatorial_imp n =
  let mutable i = n
  let mutable result = 1
  
  while i > 0 do
    result <- result * i
    i <- i - 1
  
  result

fatorial_imp 10

// Factorial with fold
let times x y = x * y
let factorial n = List.fold times 1 [1..n]
factorial 10

// Manual unzip
let unzip pairs =
  let prependPair (u, v) (l, m) = u::l, v::m
  List.foldBack prependPair pairs ([], [])

// Fibinacci with unfold
let fibSeq = Seq.unfold (fun (a,b) -> Some(a+b, (b, a+b))) (0,1)
fibSeq |> Seq.take 10 |> Seq.iter (fun el -> printfn $"{el}")
