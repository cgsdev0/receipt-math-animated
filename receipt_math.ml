open Core
open Canvas2d
open Js_of_ocaml

type t =
  | X
  | C of int
  | Y
  | Xor of t * t
  | And of t * t
  | MirrorX of t
  | MirrorY of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Mod of t * t
[@@deriving quickcheck, sexp_of, equal]

let rec simplify t =
  match t with
  | X | Y | C _ -> t
  | Sub (a, b) ->
    let a = simplify a in
    let b = simplify b in
    (match b with
     | C 0 -> a
     | _ when equal a b -> C(0)
     | _ -> Sub (a, b))
  | _ -> t
;;

(* TODO: generate this thing *)
let rec size t =
  match t with
  | X | Y | C _ -> 1
  | Xor (a, b) | And (a, b) | Add (a, b) | Sub (a, b) | Mul (a, b) | Mod (a, b) ->
    size a + size b + 1
  | MirrorX a | MirrorY a -> size a + 1
;;

let rec eval ~x ~y t =
  match t with
  | X -> x
  | Y -> y
  | C c -> c % 256
  | Xor (a, b) -> eval ~x ~y a lxor eval ~x ~y b
  | And (a, b) -> eval ~x ~y a land eval ~x ~y b
  | Add (a, b) -> (eval ~x ~y a + eval ~x ~y b) % 256
  | Sub (a, b) -> (eval ~x ~y a - eval ~x ~y b) % 256
  | Mul (a, b) -> eval ~x ~y a * eval ~x ~y b % 256
  | Mod (a, b) ->
    let a = eval ~x ~y a in
    let b = eval ~x ~y b in
    (match b with
     | 0 -> a
     | _ -> a % b)
    % 256
  | MirrorX a -> eval ~x ~y:(Int.abs (Int.abs (128 - y) - 1)) a
  | MirrorY a -> eval ~y ~x:(Int.abs (Int.abs (128 - x) - 1)) a
;;

let mirror ?(mirror_x = false) ?(mirror_y = false) image_data =
  match mirror_x, mirror_y with
  | false, false -> ()
  | _ ->
    for y = 0 to 255 do
      for x = 0 to 255 do
        if mirror_y && x >= 128
        then (
          let color = Image_data.get_r image_data ~x:(255 - x) ~y in
          Image_data.set image_data ~x ~y ~g:color ~b:color ~r:color ~a:255);
        if mirror_x && y >= 128
        then (
          let color = Image_data.get_r image_data ~y:(255 - y) ~x in
          Image_data.set image_data ~x ~y ~g:color ~b:color ~r:color ~a:255)
      done
    done
;;

let evil_thing image_data t =
  for y = 0 to 255 do
    for x = 0 to 255 do
      let color = eval ~x ~y t in
      Image_data.set image_data ~x ~y ~g:color ~b:color ~r:color ~a:255
    done
  done
;;

let () =
  let document = Dom_html.document in
  let body = document##.body in
  let c = Canvas.create ~width:256 ~height:256 in
  let ctx = Canvas.ctx2d c in
  let image_data = Ctx2d.get_image_data ctx in
  evil_thing image_data (And (X, Y));
  (* let t = MirrorY (MirrorX (Xor (Mul(X, C(2)), Y))) in *)
  (* let () = Quickcheck.random_value in *)
  let rec generate () =
    let t =
      simplify
        (Quickcheck.random_value ~size:4 ~seed:`Nondeterministic quickcheck_generator)
    in
    print_s (sexp_of_t t);
    print_s (sexp_of_int (size t));
    let s = size t in
    if s > 5 then t else generate ()
  in
  let t = generate () in
  evil_thing image_data t;
  (* evil_thing image_data (Xor (X, Y)); *)
  mirror ~mirror_x:false ~mirror_y:false image_data;
  Ctx2d.put_image_data ctx image_data ~x:0 ~y:0;
  ignore (body##appendChild (Canvas.dom_element c :> Dom.node Js.t));
  ()
;;
