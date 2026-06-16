open Core
open Canvas2d
open Js_of_ocaml

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
[@@deriving quickcheck, sexp_of, equal]

let rec simplify t =
  match t with
  | X | Y -> t
  | C t -> C(t % 256)
  | Sub (a, b) ->
    let a = simplify a in
    let b = simplify b in
    (match b with
     | C 0 -> a
     | _ when equal a b -> C 0
     | _ -> Sub (a, b))
  | Add (a, b) ->
    let a = simplify a in
    let b = simplify b in
    (match a, b with
     | C 0, _ -> b
     | _, C 0 -> a
     | _ -> Add (a, b))
  | Mod (a, b) ->
    let a = simplify a in
    let b = simplify b in
    (match b with
     | C 0 -> a
     | _ -> Mod (a, b))
  | Mul (a, b) ->
    let a = simplify a in
    let b = simplify b in
    (match a,b with
     | C 0, _ | _, C 0 -> C 0
     | x, C 1 | C 1, x -> x
     | _ -> Mul (a, b))
  | Xor (a, b) ->
    let a = simplify a in
    let b = simplify b in
    (match a,b with
     | _ when equal a b -> C 0
     | _ -> Xor (a, b))
  | And (a, b) ->
    let a = simplify a in
    let b = simplify b in
    (match a,b with
     | _ when equal a b -> a
     | _ -> And (a, b))
  | Or (a, b) ->
    let a = simplify a in
    let b = simplify b in
    (match a,b with
     | _ when equal a b -> a
     | _ -> Or (a, b))
  | MirrorX (a) ->
    let a = simplify a in
    (match a with
     | MirrorX(a) -> a
     | C(_) -> a
     | X -> a
     | _ -> MirrorX (a))
  | MirrorY (a) ->
    let a = simplify a in
    (match a with
     | MirrorY(a) -> a
     | C(_) -> a
     | Y -> a
     | _ -> MirrorY (a))
;;

(* TODO: generate this thing *)
let rec stats t =
  match t with
  | X -> 1, true, false
  | Y -> 1, false, true
  | C _ -> 1, false, false
  | Xor (a, b) | And (a, b) | Or(a, b) | Add (a, b) | Sub (a, b) | Mul (a, b) | Mod (a, b) -> (
    let size_a, x_a, y_a = stats a in
    let size_b, x_b, y_b = stats b in
    size_a + size_b + 1, x_a || x_b, y_a || y_b
  )
  | MirrorX a | MirrorY a -> (
    let size, x, y = stats a in
    size + 1, x, y
  )
;;

let rec eval ~x ~y t =
  match t with
  | X -> x
  | Y -> y
  | C c -> c % 256
  | Xor (a, b) -> eval ~x ~y a lxor eval ~x ~y b
  | Or (a, b) -> eval ~x ~y a lor eval ~x ~y b
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

let evil_thing image_data t =
  for y = 0 to 255 do
    for x = 0 to 255 do
      let color = eval ~x ~y t in
      Image_data.set image_data ~x ~y ~g:color ~b:color ~r:color ~a:255
    done
  done
;;

let remap color min max =
  let range = max - min in
  let ratio = 255.0 /. Float.of_int range in
  match range with
  | 0 -> color
  | _ -> Int.of_float (Float.of_int (color - min) *. ratio)
;;

let tonemap image_data =
  let min_value = ref 255 in
  let max_value = ref 0 in
  for y = 0 to 255 do
    for x = 0 to 255 do
      let color = Image_data.get_r image_data ~x ~y in
      if color < !min_value then min_value := color;
      if color > !max_value then max_value := color
    done
  done;
  for y = 0 to 255 do
    for x = 0 to 255 do
      let color = Image_data.get_r image_data ~x ~y in
      let color = remap color !min_value !max_value in
      Image_data.set image_data ~x ~y ~g:color ~b:color ~r:color ~a:255
    done
  done
;;

let color_ramp image_data =
  let a = Oklab.of_rgb ~r:1.0 ~g:0.0 ~b:0.0 () in
  let b = Oklab.of_rgb ~r:0.0 ~g:1.0 ~b:0.0 () in
  for y = 0 to 255 do
    for x = 0 to 255 do
      let color = Image_data.get_r image_data ~x ~y in
      let r, g, b = Oklab.to_rgb (Oklab.lerp a b ((Float.of_int color) /. 255.0)) in
      Image_data.set image_data ~x ~y ~a:255 ~b ~g ~r
    done
  done;
;;

let () =
  Js.Unsafe.global##.foo
  := Js.wrap_callback (fun () ->
       let c = Canvas.create ~width:256 ~height:256 in
       let ctx = Canvas.ctx2d c in
       let image_data = Ctx2d.get_image_data ctx in
       evil_thing image_data (And (X, Y));
       (* let t = MirrorY (MirrorX (Xor (Mul(X, C(2)), Y))) in *)
       (* let () = Quickcheck.random_value in *)
       let rec generate () =
         let t =
           simplify
             (Quickcheck.random_value
                ~size:4
                ~seed:`Nondeterministic
                quickcheck_generator)
         in
         let size, x, y = stats t in
         if size > 5 && x && y then t else generate ()
       in
       let t = generate () in
       let equation = Sexp.to_string_hum ~indent:2 ~max_width:42 (sexp_of_t t) in
       evil_thing image_data t;
       tonemap image_data;
       (* color_ramp image_data; *)
       Ctx2d.put_image_data ctx image_data ~x:0 ~y:0;
       let c2 = Canvas.create ~width:512 ~height:512 in
       let ctx2 = Canvas.ctx2d c2 in
       Ctx2d.draw_canvas ~sw:256.0 ~sh:256.0 ~w:512.0 ~h:512.0 ctx2 c ~x:0.0 ~y:0.0;
       object%js
         val c = Canvas.dom_element c2
         val e = Js.string equation
       end)
;;
