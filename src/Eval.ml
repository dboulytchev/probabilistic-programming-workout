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
    val reduce : t * float -> t * float (* c -> c *)
  end

let is_value = function
  | Num _       -> true
  | Bool _      -> true
  | Alnum _     -> true
  | Nil         -> true
  | Sample      -> true
  | Observe     -> true
  | Dist _      -> true
  | Var _       -> true
  | Abs (_, _)  -> true
  | App _       -> false
  | If (_, _, _)-> false

module Eval (R : R) =
  struct
    let rec step e w = match e with
      | App args when (List.exists (fun x -> not (is_value x)) args) -> let args', w' = args_step args w in App args', w'
      | App args                                                     -> R.reduce (App args, w)
      | If (cond, t, f) when (is_value cond)                         -> R.reduce (If (cond, t, f), w)
      | If (cond, t, f)                                              -> let e', w' = R.reduce (cond, w) in If (e', t, f), w'
      | c -> c, w
    and args_step args w = match args with
      | x::xs when is_value x -> let xs', w' = args_step xs w in x :: xs', w'
      | x::xs                 -> let x', w' = step x w in x'::xs, w
      | []                    -> [], w
  end


(* Likelihood Weighted Importance Sampler *)
module LWIS =
  struct
    let sample_dist d args = match d, args with
      | "flip", [Num p; Num t; Num f] -> if Random.float 1.0 < p then t else f
      | "uniform", [Num beg; Num len] -> beg +. Random.float len
(*       | "normal", _ -> 0.0
      | "poisson", _ -> 0.0 *)

    let score_dist d args w = match d, args, w with
      | "flip", [Num p; Num t; Num f], v -> if v = t then p else 
                                            if v = f then 1.0 -. p else 0.0
      | "uniform", _, _ -> 0.0 (* TODO: support interval restrictions *)
      | "normal", _, _ -> 0.0
      | "poisson", _, _ -> 0.0

    let reduce = function
      | If  (Bool true, e, _), w -> e, w
      | If  (_,         _, e), w -> e, w
                            
      | App (Abs (fs, e) :: args), w -> subst e fs args, w
                                      
      | App (Alnum _ as c :: args), w ->
         op c args, w
  
      | App [Sample; App (Dist d :: args)], w ->
         (* d = flip | normal | poisson *)
         Num (sample_dist d args), w
        
      | App ([Observe; App (Dist d :: args); c]), w ->
         c, w *. (score_dist d args w) 
  end

(* General Weighted Importance Sampler *) 
module GWIS =
  struct
    
  end
