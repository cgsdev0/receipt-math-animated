open! Core

type t =
  | T
  | X
  | Y
  | C of (float[@quickcheck.generator Base_quickcheck.Generator.float_inclusive 0. 1.])
  | Xor of t * t
  | And of t * t
  | Or of t * t
  | MirrorX of t
  | MirrorY of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Mod of t * t
  | Sin of t
  | Cos of t
[@@deriving quickcheck, sexp, equal]

(* Everything in this module lives in the closed unit interval [0, 1]: [X] and [Y] are the
   normalised coordinates, every constant is clamped into range, and every operator is
   chosen so that it maps [0, 1] inputs back into [0, 1] *continuously* (no jumps). This
   is the floating-point cousin of [Expr_int], where the integer ops wrap mod 256. *)
let clamp01 v = Float.max 0. (Float.min 1. v)

(* A small denominator guard so [Mod] never divides by zero. *)
let epsilon = 1e-6

(* Reflect a coordinate around the centre of the unit interval, folding [0, 1] onto
   itself: [fold 0. = 1.], [fold 0.5 = 0.], [fold 1. = 1.]. Continuous everywhere (it has
   a corner at the centre but no jump). *)
let fold v = Float.abs ((2. *. v) -. 1.)

let rec eval ~x ~y ~time t =
  match t with
  | T -> time
  | X -> x
  | Y -> y
  | C c -> clamp01 c
  (* Probabilistic XOR: 0 when the inputs agree at the extremes, 1 when they disagree,
     smoothly interpolated in between. Stays within [0, 1]. *)
  | Xor (a, b) ->
    let a = eval ~x ~y ~time a
    and b = eval ~x ~y ~time b in
    a +. b -. (2. *. a *. b)
  (* Fuzzy AND / OR via min / max: continuous, and they obey the absorption and
     idempotence laws that [simplify] relies on. *)
  | And (a, b) -> Float.min (eval ~x ~y ~time a) (eval ~x ~y ~time b)
  | Or (a, b) -> Float.max (eval ~x ~y ~time a) (eval ~x ~y ~time b)
  (* Average rather than wrapping addition: stays in range with no discontinuity. *)
  | Add (a, b) -> (eval ~x ~y ~time a +. eval ~x ~y ~time b) /. 2.
  (* Absolute difference: the continuous analogue of modular subtraction. *)
  | Sub (a, b) -> Float.abs (eval ~x ~y ~time a -. eval ~x ~y ~time b)
  | Mul (a, b) -> eval ~x ~y ~time a *. eval ~x ~y ~time b
  (* A smooth, periodic stand-in for integer [mod]: the ratio [a / b] drives a raised
     cosine, so the output sweeps [0, 1] repeatedly as [a] grows with [b] setting the
     period. No sawtooth jump like a true remainder would have. *)
  | Mod (a, b) ->
    let a = eval ~x ~y ~time a
    and b = eval ~x ~y ~time b in
    0.5 *. (1. -. Float.cos (Float.pi *. a /. (b +. epsilon)))
  (* [MirrorX] folds the [y] axis (leaving [x] untouched); [MirrorY] folds [x]. *)
  | MirrorX a -> eval ~x ~y:(fold y) ~time a
  | MirrorY a -> eval ~x:(fold x) ~y ~time a
  (* [Sin]/[Cos] sweep a full period as the argument crosses [0, 1], remapped from the
     natural [-1, 1] range of the trig functions into [0, 1]. Like every other operator
     they stay within the unit interval -- the invariant [simplify]'s identities depend
     on. *)
  | Sin a -> Float.((sin (eval ~x ~y ~time a * (2. * pi)) + 1.) / 2.)
  | Cos a -> Float.((cos (eval ~x ~y ~time a * (2. * pi)) + 1.) / 2.)
;;

(* Fold a node whose children are both constants into a single constant by evaluating it.
   The coordinates are irrelevant: [const_fold] is only ever applied to nodes both of
   whose children are [C], none of which depend on [x]/[y]. Folding through [eval]
   guarantees the result matches [eval] exactly. *)
let const_fold t = C (eval ~x:0. ~y:0. ~time:0. t)

(* Simplify a single node, assuming its children are already simplified. Every rewrite
   here is an exact algebraic identity of the operator semantics in [eval] above, so it
   preserves the evaluated value bit-for-bit. *)
let simplify_node t =
  match t with
  | T | X | Y -> t
  | C n -> C (clamp01 n)
  (* subtraction is |a - b|; every value is >= 0, so subtracting 0 (from either side) is
     the identity, and [a - a = 0] *)
  | Sub (C _, C _) -> const_fold t
  | Sub (a, C 0.) -> a
  | Sub (C 0., a) -> a
  | Sub (a, b) when equal a b -> C 0.
  (* addition is the average; a averaged with itself is itself. (Note [Add (a, C 0.)] is
   *not* [a] here: it is [a / 2].) *)
  | Add (C _, C _) -> const_fold t
  | Add (a, b) when equal a b -> a
  (* modulo (smooth); a zero numerator yields 0 regardless of the divisor *)
  | Mod (C _, C _) -> const_fold t
  | Mod (C 0., _) -> C 0.
  (* multiplication *)
  | Mul (C _, C _) -> const_fold t
  | Mul (C 0., _) | Mul (_, C 0.) -> C 0.
  | Mul (x, C 1.) | Mul (C 1., x) -> x
  (* xor; [a xor 0 = a]. ([a xor a] is *not* 0 for fractional [a], so it is left alone.) *)
  | Xor (C _, C _) -> const_fold t
  | Xor (a, C 0.) | Xor (C 0., a) -> a
  (* and = min; [min a 0 = 0], [min a 1 = a] (values are in [0, 1]), [min a a = a] *)
  | And (C _, C _) -> const_fold t
  | And (C 0., _) | And (_, C 0.) -> C 0.
  | And (a, C 1.) | And (C 1., a) -> a
  | And (a, b) when equal a b -> a
  (* absorption: [min (a, max (a, c)) = a] *)
  | And (a, Or (b, c)) when equal a b || equal a c -> a
  | And (Or (b, c), a) when equal a b || equal a c -> a
  (* idempotence: [min (a, min (b, c)) = min (b, c)] when [a] is [b] or [c] *)
  | And (a, (And (b, c) as inner)) when equal a b || equal a c -> inner
  | And ((And (b, c) as inner), a) when equal a b || equal a c -> inner
  (* or = max; [max a 1 = 1], [max a 0 = a], [max a a = a] *)
  | Or (C _, C _) -> const_fold t
  | Or (C 1., _) | Or (_, C 1.) -> C 1.
  | Or (C 0., b) -> b
  | Or (a, C 0.) -> a
  | Or (a, b) when equal a b -> a
  (* absorption: [max (a, min (a, c)) = a] *)
  | Or (a, And (b, c)) when equal a b || equal a c -> a
  | Or (And (b, c), a) when equal a b || equal a c -> a
  (* idempotence: [max (a, max (b, c)) = max (b, c)] when [a] is [b] or [c] *)
  | Or (a, (Or (b, c) as inner)) when equal a b || equal a c -> inner
  | Or ((Or (b, c) as inner), a) when equal a b || equal a c -> inner
  (* mirrors only touch one axis, so anything independent of that axis passes through *)
  | MirrorX (C _ as a) -> a
  | MirrorX X -> X
  | MirrorY (C _ as a) -> a
  | MirrorY Y -> Y
  (* sin / cos of a constant is a constant *)
  | Sin (C _) -> const_fold t
  | Cos (C _) -> const_fold t
  (* fallthrough *)
  | t -> t
;;

(* Recursively simplify [t] bottom-up: first simplify every child, then simplify the node
   itself with [simplify_node]. *)
let rec simplify t =
  let t =
    match t with
    | T | X | Y | C _ -> t
    | Xor (a, b) -> Xor (simplify a, simplify b)
    | And (a, b) -> And (simplify a, simplify b)
    | Or (a, b) -> Or (simplify a, simplify b)
    | Add (a, b) -> Add (simplify a, simplify b)
    | Sub (a, b) -> Sub (simplify a, simplify b)
    | Mul (a, b) -> Mul (simplify a, simplify b)
    | Mod (a, b) -> Mod (simplify a, simplify b)
    | MirrorX a -> MirrorX (simplify a)
    | MirrorY a -> MirrorY (simplify a)
    | Sin a -> Sin (simplify a)
    | Cos a -> Cos (simplify a)
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
  | MirrorX a | MirrorY a | Sin a | Cos a ->
    let size, x, y, time = stats a in
    size + 1, x, y, time
;;

let rec generate () =
  let t =
    simplify
      (Quickcheck.random_value ~size:4 ~seed:`Nondeterministic quickcheck_generator)
  in
  let size, x, y, time = stats t in
  if size > 5 && x && y && time then t else generate ()
;;

module For_testing = struct
  let simplify = simplify
  let stats = stats
end
