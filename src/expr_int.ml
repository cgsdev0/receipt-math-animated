open! Core

type t =
  | T
  | X
  | Y
  | C of int
  | Xor of t * t
  | And of t * t
  | Or of t * t
  | MirrorX of t
  | MirrorY of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Mod of t * t
[@@deriving quickcheck, sexp, compare, equal]

let dimension = 256

let rec to_frag_helper counter p t r =
  incr counter;
  let r1 = sprintf "r%d" !counter in
  incr counter;
  let r2 = sprintf "r%d" !counter in
  let scaled = dimension / Int.pow 2 p in
  match t with
  | T -> sprintf "int %s=time;\n" r
  | X -> sprintf "int %s=x;\n" r
  | Y -> sprintf "int %s=y;\n" r
  | C c -> sprintf "int %s=%d;\n" r c
  | Xor (a, b) ->
    sprintf
      "%s%sint %s=pmod(%s^%s,256);\n"
      (to_frag_helper counter p a r1)
      (to_frag_helper counter p b r2)
      r
      r1
      r2
  | Or (a, b) ->
    sprintf
      "%s%sint %s=pmod(%s|%s,256);\n"
      (to_frag_helper counter p a r1)
      (to_frag_helper counter p b r2)
      r
      r1
      r2
  | And (a, b) ->
    sprintf
      "%s%sint %s=pmod(%s&%s,256);\n"
      (to_frag_helper counter p a r1)
      (to_frag_helper counter p b r2)
      r
      r1
      r2
  | Sub (a, b) ->
    sprintf
      "%s%sint %s=pmod(%s-%s,256);\n"
      (to_frag_helper counter p a r1)
      (to_frag_helper counter p b r2)
      r
      r1
      r2
  | Add (a, b) ->
    sprintf
      "%s%sint %s=pmod(%s+%s,256);\n"
      (to_frag_helper counter p a r1)
      (to_frag_helper counter p b r2)
      r
      r1
      r2
  | Mul (a, b) ->
    sprintf
      "%s%sint %s=pmod(%s*%s,256);\n"
      (to_frag_helper counter p a r1)
      (to_frag_helper counter p b r2)
      r
      r1
      r2
  | Mod (a, b) ->
    sprintf
      "%s%sint %s=pmod(%s,%s);\n"
      (to_frag_helper counter p a r1)
      (to_frag_helper counter p b r2)
      r
      r1
      r2
  | MirrorX a ->
    sprintf
      "int %s=y;\ny=abs(abs(%d-y)-1);%sy=%s;\n"
      r1
      (scaled)
      (to_frag_helper counter p a r)
      r1
  | MirrorY a ->
    sprintf
      "int %s=x;\nx=abs(abs(%d-x)-1);%sx=%s;\n"
      r1
      (scaled)
      (to_frag_helper counter p a r)
      r1
  (* | MirrorY a -> *)
  (*   sprintf *)
  (*     "%sx=abs(abs(%d-x)-1);\n // %s %s %s\n" *)
  (*     (to_frag_helper counter p a r) *)
  (*     (scaled / 2) *)
  (*     r *)
  (*     r1 *)
  (*     r2 *)
;;

let to_frag p t =
  let counter = ref 0 in
  to_frag_helper counter p t "result"
;;

let rec eval ~x ~y ~time p t =
  let scaled = dimension / Int.pow 2 p in
  match t with
  | T -> Int.of_float (time *. 60.0)
  | X -> x
  | Y -> y
  | C c -> c % 256
  | Xor (a, b) -> eval ~x ~y ~time p a lxor eval ~x ~y ~time p b
  | Or (a, b) -> eval ~x ~y ~time p a lor eval ~x ~y ~time p b
  | And (a, b) -> eval ~x ~y ~time p a land eval ~x ~y ~time p b
  | Add (a, b) -> (eval ~x ~y ~time p a + eval ~x ~y ~time p b) % 256
  | Sub (a, b) -> (eval ~x ~y ~time p a - eval ~x ~y ~time p b) % 256
  | Mul (a, b) -> eval ~x ~y ~time p a * eval ~x ~y ~time p b % 256
  | Mod (a, b) ->
    let a = eval ~x ~y ~time p a in
    let b = eval ~x ~y ~time p b in
    (match a, b with
     | _, 0 -> a
     | 0, _ -> 0
     | _ -> a % b)
    % 256
  | MirrorX a -> eval ~x ~y:(Int.abs (Int.abs ((scaled / 2) - y) - 1)) ~time p a
  | MirrorY a -> eval ~y ~x:(Int.abs (Int.abs ((scaled / 2) - x) - 1)) ~time p a
;;

(* Fold a node whose children are both constants into a single constant by evaluating it.
   The coordinates and scale are irrelevant here: [const_fold] is only ever called on the
   arithmetic/bitwise nodes (never mirrors), none of which depend on [x]/[y]/[p]. Folding
   through [eval] guarantees the result matches [eval] exactly, including its modular
   arithmetic. *)
let const_fold t = C (eval ~x:0 ~y:0 ~time:0.0 0 t)

let rec contains node t =
  if equal node t then true else
    match t with
    | X | Y | C _ | T -> false
    | Xor (a, b) | And(a, b) | Or(a, b) | Add(a, b) | Sub(a, b) | Mul(a, b) | Mod(a, b) -> (contains node a || contains node b)
    | MirrorX a | MirrorY a -> contains node a
;;

(* ---------------------------------------------------------------------------
   Printer and parser.

   The concrete syntax is roughly C-like: the binary nodes are infix operators
   and the unary [Mirror*] nodes use function-call syntax. The two coordinates
   are the bare identifiers [x] and [y], and a [C n] is just its decimal
   literal (parenthesised when negative so it never glues onto a neighbouring
   operator).

     Or  -> a | b      Add -> a + b      MirrorX a -> mirrorX(a)
     Xor -> a ^ b      Sub -> a - b      MirrorY a -> mirrorY(a)
     And -> a & b      Mul -> a * b
                       Mod -> a % b

   Precedence, lowest binding to highest, follows C: [|] < [^] < [&] <
   [+ -] < [* %]. Every binary operator is left-associative. Atoms ([x], [y],
   literals, and the [mirror*] calls) bind tightest. *)

(* Binding power of a node's top constructor; higher binds tighter. Used by both
   the printer (to decide when to parenthesise) and as documentation of the
   grammar levels the parser implements. *)
let prec = function
  | T | X | Y | C _ | MirrorX _ | MirrorY _ -> 6
  | Mul _ | Mod _ -> 5
  | Add _ | Sub _ -> 4
  | And _ -> 3
  | Xor _ -> 2
  | Or _ -> 1
;;

let to_string t =
  let buf = Buffer.create 64 in
  (* [parent] is the binding power of the surrounding context: we wrap [t] in
     parens whenever it binds more loosely than its parent would allow. For a
     left-associative operator at precedence [p] we recurse into the left child
     with [parent = p] and the right child with [parent = p + 1], which forces
     parens on a same-precedence right child (e.g. [a - (b - c)]) while leaving
     left-nested chains paren-free (e.g. [a - b - c]). *)
  let rec go ~parent t =
    let p = prec t in
    let wrap = p < parent in
    if wrap then Buffer.add_char buf '(';
    let bin a op b =
      go ~parent:p a;
      Buffer.add_string buf op;
      go ~parent:(p + 1) b
    in
    (match t with
     | T -> Buffer.add_char buf 't'
     | X -> Buffer.add_char buf 'x'
     | Y -> Buffer.add_char buf 'y'
     | C c ->
       (* A bare negative literal would glue onto a preceding operator and read
          back as subtraction, so parenthesise it. [Int.to_string] keeps the
          leading sign, which round-trips even for [Int.min_value]. *)
       if c < 0
       then (
         Buffer.add_char buf '(';
         Buffer.add_string buf (Int.to_string c);
         Buffer.add_char buf ')')
       else Buffer.add_string buf (Int.to_string c)
     | MirrorX a ->
       Buffer.add_string buf "mirrorX((x,y)=>";
       go ~parent:0 a;
       Buffer.add_char buf ')'
     | MirrorY a ->
       Buffer.add_string buf "mirrorY((x,y)=>";
       go ~parent:0 a;
       Buffer.add_char buf ')'
     | Or (a, b) -> bin a " | " b
     | Xor (a, b) -> bin a " ^ " b
     | And (a, b) -> bin a " & " b
     | Add (a, b) -> bin a " + " b
     | Sub (a, b) -> bin a " - " b
     | Mul (a, b) -> bin a " * " b
     | Mod (a, b) -> bin a " % " b);
    if wrap then Buffer.add_char buf ')'
  in
  go ~parent:0 t;
  Buffer.contents buf
;;

(* Simplify a single node, assuming its children are already simplified. This step is
   non-recursive: it never calls [simplify] on subtrees, it only inspects the
   (already-simplified) immediate children to fold constants and apply algebraic
   identities. *)
let simplify_node t =
  match t with
  | X | Y | T -> t
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
  | Mod (_, C 1) -> C 0
  | Mod (a, b) when equal a b -> C 0
  (* multiplication *)
  | Mul (C _, C _) -> const_fold t
  | Mul (C 0, _) | Mul (_, C 0) -> C 0
  | Mul (x, C 1) | Mul (C 1, x) -> x
  (* exclusive or; [a lxor 0 = a] *)
  | Xor (C _, C _) -> const_fold t
  | Xor (a, C 0) | Xor (C 0, a) -> a
  | Xor (a, b) when equal a b -> C 0
  (* and; every [eval] result is in [0, 255], so [a land 255 = a] *)
  | And (C _, C _) -> const_fold t
  | And (C 0, _) | And (_, C 0) -> C 0
  | And (a, C 255) | And (C 255, a) -> a
  | And (a, b) when equal a b -> a
  (* absorption: [a land (a lor c) = a] *)
  | And (a, Or (b, c)) when equal a b || equal a c -> a
  | And (Or (b, c), a) when equal a b || equal a c -> a
  (* idempotence: [a land (a land c) = a land c] *)
  | And (a, (And (b, c) as inner)) when equal a b || equal a c -> inner
  | And ((And (b, c) as inner), a) when equal a b || equal a c -> inner
  (* or; every [eval] result is in [0, 255], so [a lor 255 = 255] *)
  | Or (C _, C _) -> const_fold t
  | Or (C 255, _) | Or (_, C 255) -> C 255
  | Or (C 0, b) -> b
  | Or (a, C 0) -> a
  | Or (a, b) when equal a b -> a
  (* absorption: [a lor (a land c) = a] *)
  | Or (a, And (b, c)) when equal a b || equal a c -> a
  | Or (And (b, c), a) when equal a b || equal a c -> a
  (* idempotence: [a lor (a lor c) = a lor c] *)
  | Or (a, (Or (b, c) as inner)) when equal a b || equal a c -> inner
  | Or ((Or (b, c) as inner), a) when equal a b || equal a c -> inner
  (* mirrors *)
  (* [MirrorX] only changes the [y] coordinate, so anything that does not depend on [y]
     passes through unchanged. Double-mirror is *not* an identity: [eval]'s reflection is
     not an involution. *)
  | MirrorX (MirrorX a)  -> MirrorX(a)
  | MirrorX a -> (if contains Y a then t else a)
  | MirrorY (MirrorY a)  -> MirrorY(a)
  | MirrorY a -> (if contains X a then t else a)
  (* fallthrough *)
  | t -> t
;;

(* Recursively simplify [t] bottom-up: first simplify every child, then simplify the node
   itself with [simplify_node]. *)
let rec simplify t =
  let t =
    match t with
    | X | Y | C _ | T -> t
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
  | T -> 1, false, false, true
  | X -> 1, true, false, false
  | Y -> 1, false, true, false
  | C _ -> 1, false, false, false
  | Xor (a, b) | And (a, b) | Or (a, b) | Add (a, b) | Sub (a, b) | Mul (a, b) | Mod (a, b)
    ->
    let size_a, x_a, y_a, t_a = stats a in
    let size_b, x_b, y_b, t_b = stats b in
    size_a + size_b + 1, x_a || x_b, y_a || y_b, t_a || t_b
  | MirrorX a | MirrorY a ->
    let size, x, y, t = stats a in
    size + 1, x, y, t
;;

let rec generate () =
  let t =
    simplify
      (Quickcheck.random_value ~size:6 ~seed:`Nondeterministic quickcheck_generator)
  in
  let size, x, y, time = stats t in
  if size > 5 && x && y && time then t else generate ()
;;

module For_testing = struct
  let simplify = simplify
  let stats = stats
end
