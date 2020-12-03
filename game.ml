open Raylib;;
(* open Printf;; *)

module Ent = struct
  type body = { pos: float * float
              ; size: float * float
              ; vel: float * float }

  type player = { body: body }
  type zombie = { body: body }

  type t = Player of player
         | Zombie of zombie

end

let makePlayer (x: float) (y: float) =
  Ent.Player { body = { pos = (x, y)
                      ; size = (32., 32.)
                      ; vel = (0., 0.) } }

module Game = struct
  type t = {
    title: string;
    entities: Ent.t list;
  }
end

let renderBody (body: Ent.body) =
  let x, y = body.pos in
  let w, h = body.size in
  draw_rectangle (int_of_float x) (int_of_float y) (int_of_float w) (int_of_float h) Color.red

let renderEntities (state: Game.t): unit =
  let render (e: Ent.t) =
    match e with
    | Ent.Player p -> p.body |> renderBody
    | _ -> ()
  in
  state.entities |> List.iter render

let updatePhysics ({pos=(x, y); size=size; vel=(vx, vy)}: Ent.body): Ent.body =
  let dt = get_frame_time () in
  { pos=(x +. vx *. dt, y +. vy *. dt); size=size;
    vel=(vx *. Float.pow 0.2 dt, vy *. Float.pow 0.2 dt) }

let updatePlayer (player: Ent.player): Ent.t =
  let getMovement ({pos=p; size=s; vel=(vx, vy)}: Ent.body): Ent.body =
    let ax = if is_key_down Key.A then -1. else (if is_key_down Key.D then 1. else 0.) in
    let ay = if is_key_down Key.W then -1. else (if is_key_down Key.S then 1. else 0.) in
    let dt = get_frame_time () in
    { pos=p; size=s;
      vel=(vx +. 1000.0 *. dt *. ax, vy +. 1000.0 *. dt *. ay) }
  in
    Ent.Player { body = player.body |> getMovement |> updatePhysics }

let updateEntities (state: Game.t): Game.t =
  let updateEnt = function
    | Ent.Player p -> updatePlayer p
    | e -> e
  in
  { state with
    entities = state.entities |> List.map updateEnt }

let setup () : Game.t =
  init_window 1280 720 "OCaml Game";
  set_target_fps 60;
  { title = "game"
  ; entities = [makePlayer 100. 100.] }

let rec loop (state: Game.t) =
  match window_should_close () with
  | true -> close_window ()
  | false ->
     begin_drawing ();
     clear_background Color.raywhite;
     draw_text state.title 100 100 20 Color.lightgray;
     end_drawing ();
     state |> renderEntities;
     state |> updateEntities |> loop

let () = setup () |> loop
