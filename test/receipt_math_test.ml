open! Core
module Expr = Receipt_math.Expr

(* The property under test: simplifying an expression must not change the value that
   [eval] produces at any coordinate. [x]/[y] are always in [0, 255]; [p] is the scale
   exponent [eval] uses (mirrors depend on it). Returns both values so callers can compare
   them or display them. *)
let eval_direct_and_simplified t ~x ~y ~p =
  let direct = Expr.eval ~x ~y p t in
  let simplified = Expr.eval ~x ~y p (Expr.For_testing.simplify t) in
  direct, simplified
;;

(* Minimal repro: [eval] returns [a] for [Mod (a, 0)], but [simplify] used to rewrite
   [Mod (_, C 0)] to [C 0], so the two disagreed. *)
let%expect_test "mod by zero is preserved" =
  let t = Expr.(Mod (X, C 0)) in
  let direct, simplified = eval_direct_and_simplified t ~x:5 ~y:0 ~p:0 in
  printf "direct=%d simplified=%d\n" direct simplified;
  [%expect {| direct=5 simplified=5 |}]
;;

(* [eval]'s reflection [abs (abs (scaled/2 - y) - 1)] is not an involution, so
   [MirrorX (MirrorX a)] is NOT [a]. At y=128, scaled=256: f(128)=1, f(1)=126, so a
   [y]-dependent body evaluates at 126, not 128. [simplify] used to cancel the double
   mirror and produce 128. *)
let%expect_test "double mirror is not cancelled" =
  let t = Expr.(MirrorX (MirrorX Y)) in
  let direct, simplified = eval_direct_and_simplified t ~x:0 ~y:128 ~p:0 in
  printf "direct=%d simplified=%d\n" direct simplified;
  [%expect {| direct=126 simplified=126 |}]
;;

(* Documents that the [const_fold] change was behavior-preserving, not a bug fix: [eval]
   reduces leaf [C]s mod 256 before combining, and [simplify] also normalizes constants
   first (via [C t -> C (t % 256)]) before folding, so the two already agreed. (Core's [%]
   is euclidean, so 0 - 1 = 255 mod 256.) *)
let%expect_test "constant folding matches eval" =
  let t = Expr.(Sub (C 256, C 1)) in
  let direct, simplified = eval_direct_and_simplified t ~x:0 ~y:0 ~p:0 in
  printf "direct=%d simplified=%d\n" direct simplified;
  [%expect {| direct=255 simplified=255 |}]
;;

(* The new algebraic identities each fire and reduce the expression. Correctness (that
   they preserve [eval]) is covered by the property test below. *)
let show t = print_s [%sexp (Expr.For_testing.simplify t : Expr.t)]

let%expect_test "xor with zero" =
  show Expr.(Xor (X, C 0));
  [%expect {| X |}];
  show Expr.(Xor (C 0, X));
  [%expect {| X |}]
;;

let%expect_test "and with all-ones" =
  show Expr.(And (X, C 255));
  [%expect {| X |}]
;;

let%expect_test "or with all-ones" =
  show Expr.(Or (X, C 255));
  [%expect {| (C 255) |}]
;;

let%expect_test "mod by one" =
  show Expr.(Mod (X, C 1));
  [%expect {| (C 0) |}]
;;

let%expect_test "and/or absorption" =
  show Expr.(And (X, Or (X, Y)));
  [%expect {| X |}];
  show Expr.(Or (X, And (Y, X)));
  [%expect {| X |}]
;;

let%expect_test "and/or idempotent nesting" =
  show Expr.(And (X, And (X, Y)));
  [%expect {| (And X Y) |}];
  show Expr.(Or (Or (Y, X), X));
  [%expect {| (Or Y X) |}]
;;

let%test_unit "simplify preserves eval" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%map t = Expr.quickcheck_generator
    and x = Int.gen_incl 0 255
    and y = Int.gen_incl 0 255
    and p = Int.gen_incl 0 8 in
    t, x, y, p
  in
  Quickcheck.test
    gen
    ~trials:1000
    ~sexp_of:[%sexp_of: Expr.t * int * int * int]
    ~f:(fun (t, x, y, p) ->
      let direct, simplified = eval_direct_and_simplified t ~x ~y ~p in
      [%test_eq: int] direct simplified)
;;
