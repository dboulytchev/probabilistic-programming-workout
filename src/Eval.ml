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
    
    let sample_dist = invalid_arg ""

    let score_dist = invalid_arg ""
  
    let reduce = function
      | If  (Bool true, e, _), w -> e, w
      | If  (_,         _, e), w -> e, w
                            
      | App (Abs (fs, e) :: args), w -> subst e fs args, w
                                      
      | App (Alnum _ as c :: args), w ->
         op c args, w
  
      | App [Sample; App (Dist d :: args)], w ->
         (* d = flip | normal | poisson *)
         sample_dist d args, w
        
      | App ([Observe; App (Dist d :: args); c]), w ->
         c, w * score_dist d args c 
  end

(* General Weighted Importance Sampler *) 
module GWIS =
  struct
    
  end
