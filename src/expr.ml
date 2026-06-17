open! Core

type t =
  | X
  | C of int
  | Y
  | Xor of t * t
  | And of t * t
  | Or of t * t
  | MirrorX of t
  | MirrorY of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Mod of t * t
[@@deriving quickcheck, sexp, equal]

let rec simplify t =
  match t with
  | X | Y -> t
  | C t -> C (t % 256)
  | Sub (a, b) ->
    let a = simplify a in
    let b = simplify b in
    (match a, b with
     | C x, C y -> C ((x - y) % 256)
     | _, C 0 -> a
     | _ when equal a b -> C 0
     | _ -> Sub (a, b))
  | Add (a, b) ->
    let a = simplify a in
    let b = simplify b in
    (match a, b with
     | C x, C y -> C ((x + y) % 256)
     | C 0, _ -> b
     | _, C 0 -> a
     | _ -> Add (a, b))
  | Mod (a, b) ->
    let a = simplify a in
    let b = simplify b in
    (match a, b with
     | _, C 0 | C 0, _ -> C 0
     | C x, C y -> C (x % y)
     | _ when equal a b -> C 0
     | _ -> Mod (a, b))
  | Mul (a, b) ->
    let a = simplify a in
    let b = simplify b in
    (match a, b with
     | C x, C y -> C (x * y % 256)
     | C 0, _ | _, C 0 -> C 0
     | x, C 1 | C 1, x -> x
     | _ -> Mul (a, b))
  | Xor (a, b) ->
    let a = simplify a in
    let b = simplify b in
    (match a, b with
     | C x, C y -> C (x lxor y % 256)
     | _ when equal a b -> C 0
     | _ -> Xor (a, b))
  | And (a, b) ->
    let a = simplify a in
    let b = simplify b in
    (match a, b with
     | C x, C y -> C (x land y % 256)
     | C 0, _ -> C 0
     | _, C 0 -> C 0
     | _ when equal a b -> a
     | _ -> And (a, b))
  | Or (a, b) ->
    let a = simplify a in
    let b = simplify b in
    (match a, b with
     | C x, C y -> C (x lor y % 256)
     | C 0, _ -> b
     | _, C 0 -> a
     | _ when equal a b -> a
     | _ -> Or (a, b))
  | MirrorX a ->
    let a = simplify a in
    (match a with
     | MirrorX a -> a
     | C _ -> a
     | X -> a
     | _ -> MirrorX a)
  | MirrorY a ->
    let a = simplify a in
    (match a with
     | MirrorY a -> a
     | C _ -> a
     | Y -> a
     | _ -> MirrorY a)
;;

let rec stats t =
  match t with
  | X -> 1, true, false
  | Y -> 1, false, true
  | C _ -> 1, false, false
  | Xor (a, b) | And (a, b) | Or (a, b) | Add (a, b) | Sub (a, b) | Mul (a, b) | Mod (a, b)
    ->
    let size_a, x_a, y_a = stats a in
    let size_b, x_b, y_b = stats b in
    size_a + size_b + 1, x_a || x_b, y_a || y_b
  | MirrorX a | MirrorY a ->
    let size, x, y = stats a in
    size + 1, x, y
;;

let dimension = 256

let rec eval ~x ~y p t =
  let scaled = dimension / Int.pow 2 p in
  match t with
  | X -> x
  | Y -> y
  | C c -> c % 256
  | Xor (a, b) -> eval ~x ~y p a lxor eval ~x ~y p b
  | Or (a, b) -> eval ~x ~y p a lor eval ~x ~y p b
  | And (a, b) -> eval ~x ~y p a land eval ~x ~y p b
  | Add (a, b) -> (eval ~x ~y p a + eval ~x ~y p b) % 256
  | Sub (a, b) -> (eval ~x ~y p a - eval ~x ~y p b) % 256
  | Mul (a, b) -> eval ~x ~y p a * eval ~x ~y p b % 256
  | Mod (a, b) ->
    let a = eval ~x ~y p a in
    let b = eval ~x ~y p b in
    (match a, b with
     | _, 0 -> a
     | 0, _ -> 0
     | _ -> a % b)
    % 256
  | MirrorX a -> eval ~x ~y:(Int.abs (Int.abs ((scaled / 2) - y) - 1)) p a
  | MirrorY a -> eval ~y ~x:(Int.abs (Int.abs ((scaled / 2) - x) - 1)) p a
;;
