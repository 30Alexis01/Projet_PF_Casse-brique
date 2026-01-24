open Lib_arkanoid

open Color
open Game
open Geometry
open Iterator
open Physic

module type Arkanoid =
sig
  include Game
  val score : scene -> int
end

module ArkanoidGame (V : Vector) (P : Physic) : Arkanoid =
struct
  module V = V
  module P = P

  type entity = Ball | Racket | Brick of int | MapBorder of bool
  (* Scene : nb balle, score, pool dyna (balle+raquete), pool stat (brick+wall) *)
  type scene = (int * int * (P.dyna, entity) P.pool * (P.stat, entity) P.pool)

  let init_scene = let stat_pool = List.fold_left
    (fun pool (body, obj) -> P.add pool body obj)
    P.empty_stat_pool
    [
      (P.stat_body (Shape.Rect (10., 5.)) (10., 10.), Brick 2);
      (P.stat_body (Shape.Rect (10., 5.)) (30., 10.), Brick 2);
      (P.stat_body (Shape.Rect (10., 5.)) (10., 18.), Brick 1);
      (P.stat_body (Shape.Rect (10., 5.)) (30., 18.), Brick 1);
      (P.stat_body (Shape.Rect (90., 90.)) (45., -45.), MapBorder false);
      (P.stat_body (Shape.Rect (90., 90.)) (-45., 45.), MapBorder false);
      (P.stat_body (Shape.Rect (90., 90.)) (135., 45.), MapBorder false);
      (P.stat_body (Shape.Rect (90., 90.)) (45., 135.), MapBorder true)
    ]
  in let dyna_pool = List.fold_left
    (fun pool (body, obj) -> P.add pool body obj)
    P.empty_dyna_pool
    [
      (P.dyna_body (Shape.Rect (20., 2.)) (45., 80.) (0., 0.) (0., 0.) false, Racket);
      (P.dyna_body (Shape.Circle 3.) (45., 70.) (0., -10.) (0., 8.) true, Ball)
    ]
  in (5, 0, dyna_pool, stat_pool)


  let update (pv, score, pool1, pool2) (mous_pos, mous_click) dt  = (*updates puis update ou est la raquette*)
    let newDynaPool, indexDynaList, collisionDynaList, indexStaticList, collisionStaticList = P.update pool1 pool2 dt
    in
      let (newPV, newScore, newDynaPool_collisions, newStaticPool ) = List.fold_left (updateFromCollisions (pv,score,newDynaPool,pool2)) (pv, score, newDynaPool, pool2) (List.combine collisionStaticList, indexStaticList)
      in 
        let newDynaPool_moved_racket = updateRacket newDynaPool_collisions mous_pos
        in
          if mous_click && P.get_cond newDynaPool_moved_racket is_ball = None then (newPV, newScore, P.add newDynaPool_moved_racket (*new ball ?*),newStaticPool)
          else (newPV,newScore,newDynaPool_moved_racket,newStaticPool)

  let is_ball e = match e with Ball -> true | _ -> false
  let is_racket e = match e with Racket -> true | _ -> false

  let updateFromCollisions (pvAcc, scoreAcc, dynaPool, staticPool) (collisonElt,collisionIndex) = match collisonElt with
  |MapBorder(true) -> (match P.get_cond dynaPool is_ball with
                      |None -> failwith "Erreur : pas de balle"
                      |Some(ballIndex) -> (pvAcc-1,scoreAcc,P.pop dynaPool ballIndex,staticPool)
                      )
  |MapBorder(false) -> (pvAcc, scoreAcc, dynaPool, staticPool)
  |Brick(brickHP) -> let newScore,newStaticPool = if brickHP = 1 then (scoreAcc+100,P.pop staticPool collisionIndex)
                    else (scoreAcc,P.set staticPool collisionIndex (let (body, _) = P.get staticPool collisionIndex in P.set staticPool collisionIndex (body, Brick (brickHP - 1))))
                    in  (pv,newScore,dynaPool,newStaticPool)

  let updateRacket dynaPool mous_pos =
    match P.get_cond dynaPool is_racket with
      | None -> failwith "a plus raquete ???"
      | Some (racketIdx, (rack_shape, (_, rack_y), _, _, _), _) -> P.set dynaPool racketIdx (P.dyna_body rack_shape (* TODO *))


  let start inputs dt = Flux.unfold (fun s -> match update s (F.uncons inputs) dt with None -> None | Some (s') -> Some (s', s')) init_scene

  let draw (_, _, pool1, pool2) f =
    let wrap = fun ((shape, pos, _, _, _) : _ P.body) obj -> f shape pos
      (match obj with
        | Ball -> Color.Cyan
        | Brick _ -> Color.Cyan
        | Racket -> Color.Black
        | MapBorder _ -> Color.White)
    in let _ = P.iter pool1 wrap
    in let _ = P.iter pool2 wrap
    in ()

    let score (_, s, _, _) = s

end