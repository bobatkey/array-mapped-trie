(* Fast mergable integer maps, Okasaki and Gill *)

module type S = sig
  type 'a t

  val empty : 'a t
  val find : int -> 'a t -> 'a
  val add : int -> 'a -> 'a t -> 'a t
end

module LittleEndian : S = struct
  type 'a t =
    | Empty
    | Leaf   of int * 'a
    | Branch of int * int * 'a t * 'a t

  let empty = Empty

  let apply_mask key mask =
    key land (mask-1)

  let match_prefix key prefix mask =
    apply_mask key mask = prefix

  let rec find key = function
    | Empty -> raise Not_found
    | Leaf (key', v) ->
      if key = key' then
        v
      else
        raise Not_found
    | Branch (prefix,mask,tl,tr) ->
      if key land mask = 0 then
        find key tl
      else
        find key tr

let lowest_bit x =
  x land (-x)

let branching_bit prefix0 prefix1 =
  lowest_bit (prefix0 lxor prefix1)

let join prefix0 t0 prefix1 t1 =
  let mask = branching_bit prefix0 prefix1 in
  if prefix0 land mask = 0 then
    Branch (apply_mask prefix0 mask, mask, t0, t1)
  else
    Branch (apply_mask prefix0 mask, mask, t1, t0)

let rec add key v = function
  | Empty ->
    Leaf (key, v)
  | Leaf (key',v') as t ->
    if key = key' then Leaf (key,v)
    else join key (Leaf (key,v)) key' t
  | Branch (prefix,mask,tl,tr) as t ->
    if match_prefix key prefix mask then
      if key land mask = 0 then
        Branch (prefix, mask, add key v tl, tr)
      else
        Branch (prefix, mask, tl, add key v tr)
    else
      join key (Leaf (key,v)) prefix t
end

module BigEndian : S = struct
  type 'a t =
    | Empty
    | Leaf   of int * 'a
    | Branch of int * int * 'a t * 'a t

  let empty = Empty

  let apply_mask key mask =
    (key lor (mask-1)) land (lnot mask)

  let match_prefix key prefix mask =
    apply_mask key mask = prefix

  let rec find key = function
    | Empty -> raise Not_found
    | Leaf (key', v) ->
      if key = key' then
        v
      else
        raise Not_found
    | Branch (prefix,mask,tl,tr) ->
      if key <= prefix then
        find key tl
      else
        find key tr

  let lowest_bit x =
    x land (-x)

  let highest_bit x guess =
    let x' = x land (lnot (guess-1)) in
    let rec high_bit x =
      let m = lowest_bit x in
      if x = m then m else high_bit (x-m)
    in
    high_bit x'

  let branching_bit prefix0 mask0 prefix1 mask1 =
    highest_bit
      (prefix0 lxor prefix1)
      (max 1 (2 * max mask0 mask1))

  let join prefix0 mask0 t0 prefix1 mask1 t1 =
    let mask = branching_bit prefix0 mask0 prefix1 mask1 in
    if prefix0 land mask = 0 then
      Branch (apply_mask prefix0 mask, mask, t0, t1)
    else
      Branch (apply_mask prefix0 mask, mask, t1, t0)

  let rec add key v = function
    | Empty ->
      Leaf (key, v)
    | Leaf (key',v') as t ->
      if key = key' then Leaf (key,v)
      else join key 0 (Leaf (key,v)) key' 0 t
    | Branch (prefix,mask,tl,tr) as t ->
      if match_prefix key prefix mask then
        if key land mask = 0 then
          Branch (prefix, mask, add key v tl, tr)
        else
          Branch (prefix, mask, tl, add key v tr)
      else
        join key 0 (Leaf (key,v)) prefix mask t
end
