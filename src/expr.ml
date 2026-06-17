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

(* Fold a node whose children are both constants into a single constant by evaluating it.
   The coordinates and scale are irrelevant here: [const_fold] is only ever called on the
   arithmetic/bitwise nodes (never mirrors), none of which depend on [x]/[y]/[p]. Folding
   through [eval] guarantees the result matches [eval] exactly, including its modular
   arithmetic. *)
let const_fold t = C (eval ~x:0 ~y:0 0 t)

(* Simplify a single node, assuming its children are already simplified. This step is
   non-recursive: it never calls [simplify] on subtrees, it only inspects the
   (already-simplified) immediate children to fold constants and apply algebraic
   identities. *)
let simplify_node t =
  match t with
  | X | Y -> t
  | C n -> C (n % 256)
  (* subtraction *)
  | Sub (C _, C _) -> const_fold t
  | Sub (a, C 0) -> a
  | Sub (a, b) when equal a b -> C 0
  (* addition *)
  | Add (C _, C _) -> const_fold t
  | Add (C 0, b) -> b
  | Add (a, C 0) -> a
  (* modulo *)
  | Mod (C _, C _) -> const_fold t
  | Mod (a, C 0) -> a
  | Mod (C 0, _) -> C 0
  | Mod (a, b) when equal a b -> C 0
  (* multiplication *)
  | Mul (C _, C _) -> const_fold t
  | Mul (C 0, _) | Mul (_, C 0) -> C 0
  | Mul (x, C 1) | Mul (C 1, x) -> x
  (* exclusive or *)
  | Xor (C _, C _) -> const_fold t
  | Xor (a, b) when equal a b -> C 0
  (* and *)
  | And (C _, C _) -> const_fold t
  | And (C 0, _) | And (_, C 0) -> C 0
  | And (a, b) when equal a b -> a
  (* or *)
  | Or (C _, C _) -> const_fold t
  | Or (C 0, b) -> b
  | Or (a, C 0) -> a
  | Or (a, b) when equal a b -> a
  (* mirrors *)
  (* [MirrorX] only changes the [y] coordinate, so anything that does not depend on [y]
     passes through unchanged. Double-mirror is *not* an identity: [eval]'s reflection is
     not an involution. *)
  | MirrorX (C _ as a) -> a
  | MirrorX X -> X
  | MirrorY (C _ as a) -> a
  | MirrorY Y -> Y
  (* fallthrough *)
  | t -> t
;;

(* Recursively simplify [t] bottom-up: first simplify every child, then simplify the node
   itself with [simplify_node]. *)
let rec simplify t =
  let t =
    match t with
    | X | Y | C _ -> t
    | Xor (a, b) -> Xor (simplify a, simplify b)
    | And (a, b) -> And (simplify a, simplify b)
    | Or (a, b) -> Or (simplify a, simplify b)
    | Add (a, b) -> Add (simplify a, simplify b)
    | Sub (a, b) -> Sub (simplify a, simplify b)
    | Mul (a, b) -> Mul (simplify a, simplify b)
    | Mod (a, b) -> Mod (simplify a, simplify b)
    | MirrorX a -> MirrorX (simplify a)
    | MirrorY a -> MirrorY (simplify a)
  in
  simplify_node t
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
