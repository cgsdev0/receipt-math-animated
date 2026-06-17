open Core
open Canvas2d
open Js_of_ocaml
module Expr = Expr

let evil_thing params image_data t =
  let scaled = Expr.dimension / Int.pow 2 (Param.scale params) in
  for y = 0 to scaled - 1 do
    for x = 0 to scaled - 1 do
      let color = Expr.eval ~x ~y (Param.scale params) t in
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

let tonemap dimension image_data =
  let min_value = ref 255 in
  let max_value = ref 0 in
  for y = 0 to dimension - 1 do
    for x = 0 to dimension - 1 do
      let color = Image_data.get_r image_data ~x ~y in
      if color < !min_value then min_value := color;
      if color > !max_value then max_value := color
    done
  done;
  for y = 0 to dimension - 1 do
    for x = 0 to dimension - 1 do
      let color = Image_data.get_r image_data ~x ~y in
      let color = remap color !min_value !max_value in
      Image_data.set image_data ~x ~y ~g:color ~b:color ~r:color ~a:255
    done
  done
;;

let color_ramp image_data params ~gradient =
  let a = Param.get_start_color params in
  let b = Param.get_end_color params in
  for y = 0 to 511 do
    for x = 0 to 511 do
      let color = Image_data.get_r image_data ~x ~y in
      let pct = Float.of_int color /. 255.0 in
      let pct =
        match gradient with
        | `linear -> pct
        | `square -> pct *. pct
        | `sqrt -> Float.sqrt pct
        | `sin -> Float.(sin (pct * pi))
        | `cos -> Float.((cos ((pct * pi) - pi) + 1.0) / 2.0)
      in
      let r, g, b = Oklab.to_rgb (Oklab.lerp a b pct) in
      Image_data.set image_data ~x ~y ~a:255 ~b ~g ~r
    done
  done
;;

let main () =
  Js.Unsafe.global##.foo
  := Js.wrap_callback (fun (program : Js.js_string Js.t Js.Optdef.t) ->
       let c = Canvas.create ~width:Expr.dimension ~height:Expr.dimension in
       let ctx = Canvas.ctx2d c in
       let image_data = Ctx2d.get_image_data ctx in
       (* let t = MirrorY (MirrorX (Xor (Mul(X, C(2)), Y))) in *)
       (* let () = Quickcheck.random_value in *)
       let rec generate () =
         let t =
           Expr.simplify
             (Quickcheck.random_value
                ~size:4
                ~seed:`Nondeterministic
                Expr.quickcheck_generator)
         in
         let size, x, y = Expr.stats t in
         if size > 5 && x && y then t else generate ()
       in
       let t =
         match Js.Optdef.to_option program with
         | None -> generate ()
         | Some s -> s |> Js.to_string |> Sexp.of_string |> [%of_sexp: Expr.t]
       in
       let equation = Sexp.to_string_hum ~indent:2 ~max_width:42 ([%sexp_of: Expr.t] t) in
       let params =
         Quickcheck.random_value ~seed:`Nondeterministic Param.quickcheck_generator
       in
       evil_thing params image_data t;
       let scaled = Expr.dimension / Int.pow 2 (Param.scale params) in
       tonemap scaled image_data;
       Ctx2d.put_image_data ctx image_data ~x:0 ~y:0;
       (* Copy 1 - upscaled *)
       let c2 = Canvas.create ~width:512 ~height:512 in
       let ctx2 = Canvas.ctx2d c2 in
       Ctx2d.draw_canvas
         ~sw:(Float.of_int scaled)
         ~sh:(Float.of_int scaled)
         ~w:512.0
         ~h:512.0
         ctx2
         c
         ~x:0.0
         ~y:0.0;
       (* Copy 2 - colorized *)
       let c3 = Canvas.create ~width:512 ~height:512 in
       let ctx3 = Canvas.ctx2d c3 in
       Ctx2d.draw_canvas
         ~sw:(Float.of_int scaled)
         ~sh:(Float.of_int scaled)
         ~w:512.0
         ~h:512.0
         ctx3
         c
         ~x:0.0
         ~y:0.0;
       let image_data = Ctx2d.get_image_data ctx3 in
       color_ramp image_data params ~gradient:(Param.gradient params);
       Ctx2d.put_image_data ctx3 image_data ~x:0 ~y:0;
       let gradient_str =
         params
         |> Param.gradient
         |> [%sexp_of: [ `linear | `square | `sqrt | `sin | `cos ]]
         |> Sexp.to_string
       in
       object%js
         val c = Canvas.dom_element c2
         val colored = Canvas.dom_element c3

         val e =
           Js.string
             (equation
              ^ sprintf "\nScale: %d\nGradient: %s" (Param.scale params) gradient_str)
       end)
;;
