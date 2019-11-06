open GT
  
@type t =
| Num    of float
| Bool   of bool
| Alnum  of string
| Nil
| Sample
| Observe
| Dist   of string
| Var    of string
| Abs    of string list * t
| App    of t list
| If     of t * t * t with show, eval

let op (Alnum s) =
  match s with
  | "+"   -> fun [Num  x; Num  y] -> Num  (x +. y)
  | "-"   -> fun [Num  x; Num  y] -> Num  (x -. y)
  | "*"   -> fun [Num  x; Num  y] -> Num  (x *. y)
  | "/"   -> fun [Num  x; Num  y] -> Num  (x /. y)
  | "and" -> fun [Bool x; Bool y] -> Bool (x && y)
  | "or"  -> fun [Bool x; Bool y] -> Bool (x || y)
