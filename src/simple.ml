
(* #require "podge" *)

(* let () = *)
(*   Podge.Unix.read_process_output "ls -halt" *)
(*   |> List.iter print_endline *)

open Core.Std

type myType =
    A
  | B ;;

let f = function
    A -> 0
  | B -> 1;;



module type X_int = sig val x : int end;;

module Increment (M : X_int) : X_int = struct
  let x = M.x + 1
end;;

module Three = struct let x = 3 end;;

module Four = Increment(Three);;

module Three_and_more = struct
  let x = 3
  let y = "y"
end;;

module Four = Increment(Three_and_more);;

 (* ---------------------------- *)

module Interval = struct
  type t = | Interval of int * int
           | Empty

  let create low heigh =
    if heigh < low then Empty else Interval (low, heigh)
end ;;

module Extended_Inteval = struct
  include Interval

  let contains t x =
    match t with
    | Empty -> false
    | Interval (low, heigh) -> x >= low && x <= heigh
end;;

(* ---------------------------- *)
module type Comparable = sig
  type t
  val compare : t -> t -> int
end ;;

module type Interval_intf = sig
   type t
   type endpoint
   val create : endpoint -> endpoint -> t
   val is_empty : t -> bool
   val contains : t -> endpoint -> bool
   val intersect : t -> t -> t
end;;

module Make_interval(Endpoint : Comparable)
  : (Interval_intf with type endpoint := Endpoint.t)
=  struct
  (* type endpoint = Endpoint.t *)
  type t = | Interval of Endpoint.t * Endpoint.t
           | Empty

  let create low heigh =
    if Endpoint.compare low heigh  > 0 then Empty
    else Interval (low, heigh)

  let is_empty = function
    | Empty -> true
    | Interval _ -> false

  let contains t x =
    match t with
    | Empty -> false
    | Interval (l, h) ->
      Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match t1, t2 with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1, h1), Interval(l2, h2) -> create (max l1 l2) (min h1 h2)

end ;;

module Int_interval =
  Make_interval(Int);;

let i1 = Int_interval.create 3 4;;
