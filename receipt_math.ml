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

module type Bounds = sig
  val low : float
  val high : float
end

module Bound_float (B : Bounds) = struct
  include Float

  let quickcheck_generator = Base_quickcheck.Generator.float_inclusive B.low B.high
end

module Z_to_one = Bound_float (struct
    let low = 0.0
    let high = 1.0
  end)

module Lightness = Bound_float (struct
    let low = 0.0
    let high = 1.0
  end)

module Lightness_delta = Bound_float (struct
    let low = 0.25
    let high = 0.75
  end)

module Chroma = Bound_float (struct
    let low = 0.0
    let high = 0.3
  end)

module Chroma_delta = Bound_float (struct
    let low = 0.1
    let high = 0.2
  end)

module Hue = Bound_float (struct
    let low = 0.0
    let high = 360.0
  end)

module Hue_delta = Bound_float (struct
    let low = 0.0
    let high = 360.0
  end)

type param =
  { lightness : Lightness.t
  ; lightness_delta : Lightness_delta.t
  ; chroma : Chroma.t
  ; chroma_delta : Chroma_delta.t
  ; hue : Hue.t
  ; hue_delta : Hue_delta.t
  }
[@@deriving quickcheck, sexp_of, equal]

let get_start_color' { lightness; chroma; hue; _ } =
  Oklab.Lch.create ~l:lightness ~c:chroma ~h:hue ()
;;

let get_end_color' { lightness; lightness_delta; chroma; chroma_delta; hue; hue_delta } =
  let open Float in
  let l = (lightness + lightness_delta) % 1.0 in
  let c = (chroma + chroma_delta) % 0.3 in
  let h = (hue + hue_delta) % 360.0 in
  Oklab.Lch.create ~l ~c ~h ()
;;

let get_start_color params =
  let a = get_start_color' params in
  let b = get_end_color' params in
  if Float.(Oklab.Lch.lightness a > Oklab.Lch.lightness b)  then
    b else a
;;
let get_end_color params =
  let a = get_start_color' params in
  let b = get_end_color' params in
  if Float.(Oklab.Lch.lightness a > Oklab.Lch.lightness b)  then
    a else b
;;

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

(* TODO: generate this thing *)
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
    (match a, b with
     | _, 0 -> a
     | 0, _ -> 0
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

let color_ramp image_data params =
  let a = params |> get_start_color |> Oklab.Lch.to_lab in
  let b = params |> get_end_color |> Oklab.Lch.to_lab in
  for y = 0 to 511 do
    for x = 0 to 511 do
      let color = Image_data.get_r image_data ~x ~y in
      let r, g, b = Oklab.to_rgb (Oklab.lerp a b (Float.of_int color /. 255.0)) in
      Image_data.set image_data ~x ~y ~a:255 ~b ~g ~r
    done
  done
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
       let params =
         Quickcheck.random_value ~seed:`Nondeterministic quickcheck_generator_param
       in
       evil_thing image_data t;
       tonemap image_data;
       Ctx2d.put_image_data ctx image_data ~x:0 ~y:0;
       (* Copy 1 - upscaled *)
       let c2 = Canvas.create ~width:512 ~height:512 in
       let ctx2 = Canvas.ctx2d c2 in
       Ctx2d.draw_canvas ~sw:256.0 ~sh:256.0 ~w:512.0 ~h:512.0 ctx2 c ~x:0.0 ~y:0.0;
       (* Copy 2 - colorized *)
       let c3 = Canvas.create ~width:512 ~height:512 in
       let ctx3 = Canvas.ctx2d c3 in
       Ctx2d.draw_canvas ~sw:256.0 ~sh:256.0 ~w:512.0 ~h:512.0 ctx3 c ~x:0.0 ~y:0.0;
       let image_data = Ctx2d.get_image_data ctx3 in
       color_ramp image_data params;
       Ctx2d.put_image_data ctx3 image_data ~x:0 ~y:0;
       object%js
         val c = Canvas.dom_element c2
         val colored = Canvas.dom_element c3
         val e = Js.string equation
       end)
;;
