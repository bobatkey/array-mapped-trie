type 'a item =
  | Subtree of int * 'a item array
  | Value   of int * 'a

type 'a t = int * 'a item array

(* FIXME: would be nice if the array was packed:

   - type 'a t = int * Obj.t array

   - (map,t)

   - t.(3*offset) contains value or submap tag
   - t.(3*offset+1) contains either 'key' or 'map'
   - t.(3*offset+2) contains either pointer to 'a or pointer to subtree
*)

(* http://en.wikipedia.org/wiki/Hamming_weight#Language_support *)
let ctpop map =
  let sk5  = 0x55555555 in
  let sk3  = 0x33333333 in
  let skf0 = 0x0f0f0f0f in

  let map = map - ((map lsr 1) land sk5) in
  let map = (map land sk3) + ((map lsr 2) land sk3) in
  let map = (map + map lsr 4) land skf0 in
  let map = map + map lsr 8 in
  let map = map + map lsr 16 in
  let map = map + map lsr 32 in
  map land 0x7f

let empty = 0, [| |]

let is_empty (map,_) = map = 0

let find key (map,t) =
  let rec search skey map t =
    let bottom5 = skey land 0x1f in
    if map land (1 lsl bottom5) = 0 then
      None
    else
      let offset = ctpop (map land ((1 lsl bottom5) - 1)) in
      match Array.unsafe_get t offset with
        | Value (key', v) when key = key' -> Some v
        | Value (key', v) -> None
        | Subtree (map,t) -> search (skey lsr 5) map t
  in
  search key map t

let remove key (map,t) =
  let rec remove skey map t =
    let bottom5 = skey land 0x1f in
    if map land (1 lsl bottom5) = 0 then
      map, t
    else
      let offset = ctpop (map land ((1 lsl bottom5) - 1)) in
      match Array.unsafe_get t offset with
        | Value (key', _) when key = key' ->
          map land (lnot (1 lsl bottom5)),
          Array.init (ctpop map - 1) begin fun i ->
            if i < offset then Array.unsafe_get t i
            else Array.unsafe_get t (i+1)
          end
        | Value _ ->
          map, t
        | Subtree (submap, subt) ->
          let submap, subt = remove (skey lsr 5) submap subt in
          if submap = 0 then
            map land (lnot (1 lsl bottom5)),
            Array.init (ctpop map - 1) begin fun i ->
              if i < offset then Array.unsafe_get t i
              else Array.unsafe_get t (i+1)
            end
          else match subt with
            | [| Value (key', v') |] ->
              let t' = Array.copy t in
              Array.unsafe_set t' offset (Value (key', v'));
              map, t'
            | _ ->
              let t' = Array.copy t in
              Array.unsafe_set t' offset (Subtree (submap, subt));
              map, t'
  in
  remove key map t

let rec make_subtree key1 skey1 v1 key2 skey2 v2 =
  let bottom5_1 = skey1 land 0x1f in
  let bottom5_2 = skey2 land 0x1f in
  if bottom5_1 = bottom5_2 then
    let skey1 = skey1 lsr 5 in
    let skey2 = skey2 lsr 5 in
    Subtree (1 lsl bottom5_1,
             [| make_subtree key1 skey1 v1 key2 skey2 v2 |])
  else
    Subtree ((1 lsl bottom5_1) lor (1 lsl bottom5_2),
             if bottom5_1 < bottom5_2 then
               [| Value (key1, v1); Value (key2, v2) |]
             else
               [| Value (key2, v2); Value (key1, v1) |])

let add key v (map,t) =
  let rec insertion d skey map t =
    let bottom5 = skey land 0x1f in
    let offset  = ctpop (map land ((1 lsl bottom5) - 1)) in
    if map land (1 lsl bottom5) = 0 then
      map lor (1 lsl bottom5),
      (* FIXME: use Array.blit? *)
      Array.init (ctpop map + 1) begin fun i ->
        if i < offset then Array.unsafe_get t i
        else if i = offset then Value (key, v)
        else Array.unsafe_get t (i-1)
      end
    else
      let t' = Array.copy t in
      begin
        match Array.unsafe_get t' offset with
          | Value (key', v') when key = key' ->
            Array.unsafe_set t' offset (Value (key, v))
          | Value (key', v') ->
            Array.unsafe_set t' offset
              (make_subtree key (skey lsr 5) v key' (key' lsr (5*d)) v')
          | Subtree (submap, subt) ->
            let submap, subt = insertion (d+1) (skey lsr 5) submap subt in
            Array.unsafe_set t' offset (Subtree (submap, subt))
      end;
      map, t'
  in
  insertion 1 key map t

let singleton key v =
  add key v empty

let iter f (_,t) =
  let rec visit t =
    Array.iter (function
      | Value (key, v) -> f key v
      | Subtree (_, t) -> visit t) t
  in
  visit t

let fold f a (_,t) =
  let accum = ref a in
  let rec visit t =
    Array.iter (function
      | Value (key, v) -> accum := f !accum key v
      | Subtree (_, t) -> visit t) t
  in
  visit t; !accum

(*
let adjust k f (map,t) =
  failwith "FIXME: Look for 'k' in 't'. If found, replace with 'f (Some x)', otherwise insert 'f None'"
*)

(*
module HAMT = struct
  type ('a,'b) bucket =
    | Empty
    | Bucket of 'a * 'b * ('a, 'b) bucket

  type ('a,'b) t = ('a,'b) bucket AMT.t

  let empty = AMT.empty

  let update_bucket a b x =
    assert false

  let add a b t =
    let k = Hashtbl.hash a in
    match AMT.find k t with
      | None   -> AMT.add k (Bucket (a,b,Empty)) t
      | Some x -> AMT.add k (update_bucket a b x) t

  let find a t =
    

end
*)
