type 'a item =
  | Subtree of int * 'a item array
  | Value   of int * 'a

type 'a t = { map   : int
            ; items : 'a item array
            }

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

let empty = { map = 0; items = [| |] }

let is_empty {map} = map = 0

let find key {map;items=t} =
  let rec search skey map t =
    let bottom5 = skey land 0x1f in
    if map land (1 lsl bottom5) = 0 then
      raise Not_found
    else
      let offset = ctpop (map land ((1 lsl bottom5) - 1)) in
      match Array.unsafe_get t offset with
        | Value (key', v) when key = key' -> v
        | Value (key', v) -> raise Not_found
        | Subtree (map,t) -> search (skey lsr 5) map t
  in
  search key map t

let remove key {map;items=t} =
  let rec remove skey map t =
    let bottom5 = skey land 0x1f in
    if map land (1 lsl bottom5) = 0 then
      {map;items=t}
    else
      let offset = ctpop (map land ((1 lsl bottom5) - 1)) in
      match Array.unsafe_get t offset with
        | Value (key', _) when key = key' ->
          { map   = map land (lnot (1 lsl bottom5))
          ; items =
              Array.init (Array.length t - 1) begin fun i ->
                if i < offset then Array.unsafe_get t i
                else Array.unsafe_get t (i+1)
              end
          }
        | Value _ ->
          {map;items=t}
        | Subtree (submap, subt) ->
          let {map=submap;items=subt} = remove (skey lsr 5) submap subt in
          if submap = 0 then
            { map   = map land (lnot (1 lsl bottom5))
            ; items =
                Array.init (Array.length t - 1) begin fun i ->
                  if i < offset then Array.unsafe_get t i
                  else Array.unsafe_get t (i+1)
                end
            }
          else match subt with
            | [| Value (key', v') |] ->
              let t' = Array.copy t in
              Array.unsafe_set t' offset (Value (key', v'));
              {map;items=t'}
            | _ ->
              let t' = Array.copy t in
              Array.unsafe_set t' offset (Subtree (submap, subt));
              {map;items=t'}
  in
  remove key map t

let rec set_subtree key1 skey1 v1 key2 skey2 v2 ar i =
  let bottom5_1 = skey1 land 0x1f in
  let bottom5_2 = skey2 land 0x1f in
  if bottom5_1 = bottom5_2 then
    let skey1 = skey1 lsr 5 in
    let skey2 = skey2 lsr 5 in
    let ar'    = [|Obj.magic 0|] in
    Array.unsafe_set ar i (Subtree (1 lsl bottom5_1, ar'));
    set_subtree key1 skey1 v1 key2 skey2 v2 ar' 0
  else
    Array.unsafe_set ar i
      (Subtree ((1 lsl bottom5_1) lor (1 lsl bottom5_2),
                if bottom5_1 < bottom5_2 then
                  [| Value (key1, v1); Value (key2, v2) |]
                else
                  [| Value (key2, v2); Value (key1, v1) |]))

let add key v {map;items} =
  let rec insertion d skey map t =
    let bottom5 = skey land 0x1f in
    let offset  = ctpop (map land ((1 lsl bottom5) - 1)) in
    if map land (1 lsl bottom5) = 0 then
      { map   = map lor (1 lsl bottom5)
      ; items =
          let l  = Array.length t in
          let t' = Array.make (l + 1) (Obj.magic 0) in
          Array.blit t 0 t' 0 offset;
          Array.unsafe_set t' offset (Value (key, v));
          Array.blit t offset t' (offset+1) (l - offset);
          t'
      (*  Array.init (ctpop map + 1) begin fun i ->
            if i < offset then Array.unsafe_get t i
            else if i = offset then Value (key, v)
            else Array.unsafe_get t (i-1)
          end*)
      }
    else
      let t' = (*Array.copy*) t in
      begin
        match Array.unsafe_get t' offset with
          | Value (key', v') when key = key' ->
            Array.unsafe_set t' offset (Value (key, v))
          | Value (key', v') ->
            set_subtree key (skey lsr 5) v key' (key' lsr (5*d)) v' t' offset
          | Subtree (submap, subt) ->
            let {map=submap;items=subt} = insertion (d+1) (skey lsr 5) submap subt in
            Array.unsafe_set t' offset (Subtree (submap, subt))
      end;
      {map; items=t'}
  in
  insertion 1 key map items

let singleton key v =
  add key v empty

let iter f {items=t} =
  let rec visit t =
    Array.iter (function
      | Value (key, v) -> f key v
      | Subtree (_, t) -> visit t) t
  in
  visit t

let fold f a {items=t} =
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
