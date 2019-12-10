open GT
open Syntax
   
let subst e fs args =
  let rec occurs x = function
  | []                -> false
  | y :: _ when y = x -> true
  | _ :: y            -> occurs x y
  in
  transform(t)
    (fun f -> object inherit [_, _, _] @t[eval] f
                method c_Var s _ x    = List.assoc x s
                method c_Abs s _ fs e = Abs (fs, f (List.filter (fun (f, _) -> not @@ occurs f fs) s) e) 
              end
    )
    (List.combine fs args)
    e

module type R =
  sig
    type c
    val reduce : c -> c
  end

module Eval (R : R) =
  struct

(*    let step c = function
    | 
    | x -> x
 *)       
  end
  
(* Likelihood Weighted Importance Sampler *)
module LWIS =
  struct
    let sample_dist = function 
      | "flip", [Num p; Num t; Num f] -> if Random.float 1.0 < p then t else f
      | "uniform", [Num beg; Num len] -> beg +. Random.float len

    let score_dist = function
      | "flip", [Num p; Num t; Num f], v -> if (v -. t < 0.0001) then t else 
                                            if (v -. f < 0.0001) then f else 0.0
      | "uniform", _, _ -> 0.0 (* TODO: support interval restrictions *)

    let reduce = function
      | If  (Bool true, e, _), w -> e, w
      | If  (_,         _, e), w -> e, w
                            
      | App (Abs (fs, e) :: args), w -> subst e fs args, w
                                      
      | App (Alnum _ as c :: args), w ->
         op c args, w
  
      | App [Sample; App (Dist d :: args)], w ->
         (* d = flip | normal | poisson *)
         Num (sample_dist (d, args)), w
        
      | App ([Observe; App (Dist d :: args); c]), w ->
         c, w * score_dist (d, args, c) 
  end

(* General Weighted Importance Sampler *) 
module GWIS =
  struct
    
  end
