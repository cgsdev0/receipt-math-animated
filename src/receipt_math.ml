open Core
open Canvas2d
open Js_of_ocaml
module Expr_int = Expr_int
module Expr_float = Expr_float

(* The generated program is either an integer or a floating-point expression;
   [Param.numeric] decides which one this run uses. *)
type expr =
  | Int of Expr_int.t
  | Float of Expr_float.t

(* The side length of the canvases we display. *)
let output_dimension = 512

(* Evaluate the program into a square float grid of values in [0, 1] (row-major),
   returning the grid together with its side length. The whole pipeline downstream works
   on this float buffer rather than on bytes, so values keep full floating-point precision
   until the final write to a canvas.

   The two evaluators are sampled differently, returning the [`filter] to upscale with:
   - [Expr_int] is modular byte art defined on an integer grid of coordinates in [0, 255],
     so it is rendered at its native [scaled] resolution and upscaled with [`nearest] to
     keep the characteristic blocky pixels.
   - [Expr_float] is continuous and defined on the whole unit square, so we evaluate it
     directly at the full [output_dimension], one sample per displayed pixel -- there is
     no low-resolution intermediate and therefore nothing to upsample (its [`bilinear]
     filter is an identity at this resolution). *)
let render params t time =
  match t with
  | Int t ->
    let p = Param.scale params in
    let size = Expr_int.dimension / Int.pow 2 p in
    let grid =
      Array.init (size * size) ~f:(fun i ->
        Float.of_int (Expr_int.eval ~x:(i % size) ~y:(i / size) ~time p t) /. 255.)
    in
    grid, size, `nearest
  | Float t ->
    let size = output_dimension in
    let denom = Float.of_int (size - 1) in
    let grid =
      Array.init (size * size) ~f:(fun i ->
        let fx = Float.of_int (i % size) /. denom in
        let fy = Float.of_int (i / size) /. denom in
        Expr_float.eval ~x:fx ~y:fy ~time t)
    in
    grid, size, `bilinear
;;

(* Linearly stretch the grid so its values span the full [0, 1] range. Done in floating
   point to avoid the banding that a 256-level byte round-trip would introduce. *)
let tonemap grid =
  let min_value = Array.fold grid ~init:Float.infinity ~f:Float.min in
  let max_value = Array.fold grid ~init:Float.neg_infinity ~f:Float.max in
  let range = max_value -. min_value in
  if Float.equal range 0.
  then grid
  else Array.map grid ~f:(fun v -> (v -. min_value) /. range)
;;

(* Sample a [size]x[size] grid at continuous source coordinates [fx]/[fy], clamping at the
   edges. [`nearest] snaps to the closest cell (blocky upscaling, used for the integer
   art); [`bilinear] interpolates the four neighbours, kept in floating point so the
   colour ramp below sees a smooth signal rather than a 256-level one. *)
let sample grid ~size ~filter ~fx ~fy =
  let clampi v = Int.max 0 (Int.min (size - 1) v) in
  let at x y = grid.((clampi y * size) + clampi x) in
  match filter with
  | `nearest ->
    at (Float.to_int (Float.round_nearest fx)) (Float.to_int (Float.round_nearest fy))
  | `bilinear ->
    let x0f = Float.round_down fx
    and y0f = Float.round_down fy in
    let x0 = Float.to_int x0f
    and y0 = Float.to_int y0f in
    let tx = fx -. x0f
    and ty = fy -. y0f in
    let top = (at x0 y0 *. (1. -. tx)) +. (at (x0 + 1) y0 *. tx) in
    let bot = (at x0 (y0 + 1) *. (1. -. tx)) +. (at (x0 + 1) (y0 + 1) *. tx) in
    (top *. (1. -. ty)) +. (bot *. ty)
;;

let apply_gradient gradient pct =
  match gradient with
  | `linear -> pct
  | `square -> pct *. pct
  | `sqrt -> Float.sqrt pct
  | `sin -> Float.(sin (pct * pi))
  | `cos -> Float.((cos ((pct * pi) - pi) + 1.0) / 2.0)
;;

(* Map a source grid coordinate onto the [size]x[size] grid for an output pixel [i] of an
   [out]-wide image, using pixel-centre alignment. When [size = out] (the float path) this
   is the identity, so no interpolation happens. *)
let source_coord ~size ~out i =
  ((Float.of_int i +. 0.5) *. (Float.of_int size /. Float.of_int out)) -. 0.5
;;

(* Render the colourised [out]x[out] output directly from the float grid: for each output
   pixel we bilinearly sample the (tonemapped) grid, shape it with the gradient curve, and
   map it through the Oklab colour ramp. Only the final Oklab->rgb step quantises to
   bytes, so the gradient is smooth instead of stepping through 256 grey levels. *)
let color_ramp image_data params grid ~size ~out ~filter ~gradient =
  let a = Param.get_start_color params in
  let b = Param.get_end_color params in
  for y = 0 to out - 1 do
    let fy = source_coord ~size ~out y in
    for x = 0 to out - 1 do
      let fx = source_coord ~size ~out x in
      let pct = apply_gradient gradient (sample grid ~size ~filter ~fx ~fy) in
      let r, g, b = Oklab.to_rgb (Oklab.lerp a b pct) in
      Image_data.set image_data ~x ~y ~a:255 ~b ~g ~r
    done
  done
;;

(* Render the [out]x[out] greyscale preview from the float grid. The preview is displayed
   as bytes regardless, so quantising the grey level here costs nothing. *)
let to_greyscale image_data grid ~size ~out ~filter =
  for y = 0 to out - 1 do
    let fy = source_coord ~size ~out y in
    for x = 0 to out - 1 do
      let fx = source_coord ~size ~out x in
      let c = Float.iround_nearest_exn (sample grid ~size ~filter ~fx ~fy *. 255.) in
      Image_data.set image_data ~x ~y ~r:c ~g:c ~b:c ~a:255
    done
  done
;;

let main () =
  Js.Unsafe.global##.genProgram
  := Js.wrap_callback (fun (paramstr : Js.js_string Js.t Js.Optdef.t) (program : Js.js_string Js.t Js.Optdef.t) ->
       let params = match Js.Optdef.to_option paramstr with
       | None -> Param.random ()
       | Some s -> [%of_sexp: Param.t] (s |> Js.to_string |> Sexp.of_string)
       in
       let t =
         match Js.Optdef.to_option program with
         | None ->
           (match Param.numeric params with
            | `int -> Int (Expr_int.generate ()))
           (* | `float -> Float (Expr_float.generate ())) *)
         | Some s ->
           let s = s |> Js.to_string |> Sexp.of_string in
           (match Param.numeric params with
            | `int -> Int ([%of_sexp: Expr_int.t] s))
            (* | `float -> Float ([%of_sexp: Expr_float.t] s)) *)
       in
       let equation =
         match t with
         | Int t -> Sexp.to_string_hum ~indent:2 ~max_width:42 ([%sexp_of: Expr_int.t] t)
         (* | Float t -> *)
         (*   Sexp.to_string_hum ~indent:2 ~max_width:42 ([%sexp_of: Expr_float.t] t) *)
       in
       let out = output_dimension in
       let r, g, b = Oklab.to_rgb (Param.get_start_color params) in
       let r2, g2, b2 = Oklab.to_rgb (Param.get_end_color params) in
       let c2 = Canvas.create ~width:out ~height:out in
       let c3 = Canvas.create ~width:out ~height:out in
       object%js
         val paramsexp = params |> [%sexp_of: Param.t] |> Sexp.to_string |> Js.string
         val start_color = Js.array [| r; g; b |]
         val end_color = Js.array [| r2; g2; b2 |]
         val params = Js.Unsafe.inject params
         val t = Js.Unsafe.inject t
         val c2 = Js.Unsafe.inject c2
         val c3 = Js.Unsafe.inject c3

         val frag =
           Js.string
             (match t with
              | Int t -> Expr_int.to_frag (Param.scale params) t
              | Float _ -> failwith "Expr_float not implemented yet")

         val e =
           Js.string
             (equation)
       end);
  Js.Unsafe.global##.foo
  := Js.wrap_callback
       (fun
           (timestamp : float)
           (handle :
             < params : Param.t Js.readonly_prop
             ; c2 : Canvas.t Js.readonly_prop Js.t
             ; c3 : Canvas.t Js.readonly_prop Js.t
             ; t : expr Js.readonly_prop >
               Js.t)
         ->
          let params : Param.t = Obj.magic handle##.params in
          let c2 : Canvas.t = Obj.magic (Js.Unsafe.get handle (Js.string "c2")) in
          let c3 : Canvas.t = Obj.magic (Js.Unsafe.get handle (Js.string "c3")) in
          let out = output_dimension in
          let t : expr = Obj.magic handle##.t in
          let grid, size, filter = render params t timestamp in
          let grid = tonemap grid in
          (* Copy 1 - greyscale preview *)
          let ctx2 = Canvas.ctx2d ~will_read_frequently:true c2 in
          let image_data = Ctx2d.get_image_data ctx2 in
          to_greyscale image_data grid ~size ~out ~filter;
          Ctx2d.put_image_data ctx2 image_data ~x:0 ~y:0;
          (* Copy 2 - colorized *)
          let ctx3 = Canvas.ctx2d ~will_read_frequently:true c3 in
          let image_data = Ctx2d.get_image_data ctx3 in
          color_ramp
            image_data
            params
            grid
            ~size
            ~out
            ~filter
            ~gradient:(Param.gradient params);
          Ctx2d.put_image_data ctx3 image_data ~x:0 ~y:0)
;;
