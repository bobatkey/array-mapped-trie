type 'a t = { map   : int
            ; items : Obj.t array
            }

(* - t.(3*offset) contains value (0) or submap (1) tag
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

let empty =
  { map = 0; items = [| |] }

let find key {map;items} =
  let rec search skey map items =
    let bottom5 = skey land 0x1f in
    let two_bottom5 = 1 lsl bottom5 in
    if map land two_bottom5 = 0 then
      raise Not_found
    else
      let offset = ctpop (map land (two_bottom5 - 1)) * 3 in
      match Obj.obj (Array.unsafe_get items offset) with
        | 0 when Obj.obj (Array.unsafe_get items (offset + 1)) = key ->
          Obj.obj (Array.unsafe_get items (offset + 2))
        | 0 ->
          raise Not_found
        | _ ->
          let map  = Obj.obj (Array.unsafe_get items (offset + 1)) in
          let subt = Obj.obj (Array.unsafe_get items (offset + 2)) in
          search (skey lsr 5) map subt
  in
  search key map items

let rec set_subtree key1 skey1 v1 key2 skey2 v2 ar i =
  let bottom5_1 = skey1 land 0x1f in
  let bottom5_2 = skey2 land 0x1f in
  if bottom5_1 = bottom5_2 then
    let skey1 = skey1 lsr 5 in
    let skey2 = skey2 lsr 5 in
    let ar'   = Obj.obj (Obj.new_block 0 3) in
    Array.unsafe_set ar i     (Obj.repr 1);
    Array.unsafe_set ar (i+1) (Obj.repr (1 lsl bottom5_1));
    Array.unsafe_set ar (i+2) (Obj.repr ar');
    set_subtree key1 skey1 v1 key2 skey2 v2 ar' 0
  else begin
    Array.unsafe_set ar i (Obj.repr 1);
    Array.unsafe_set ar (i+1)
      (Obj.repr ((1 lsl bottom5_1) lor (1 lsl bottom5_2)));
    Array.unsafe_set ar (i+2)
      (Obj.repr
         (if bottom5_1 < bottom5_2 then
             [| Obj.repr 0; Obj.repr key1; Obj.repr v1; 
                Obj.repr 0; Obj.repr key2; Obj.repr v2 |]
          else
             [| Obj.repr 0; Obj.repr key2; Obj.repr v2; 
                Obj.repr 0; Obj.repr key1; Obj.repr v1 |]))
  end

external unsafe_blit : 'a array -> int -> 'a array -> int -> int -> unit = "caml_array_blit"

(* FIXME: try a threshold (8?) for expanding the array to be the full 32*3 *)
let add key v {map;items} =
  let rec insertion d skey map t =
    let bottom5     = skey land 0x1f in
    let two_bottom5 = 1 lsl bottom5 in
    let offset      = ctpop (map land (two_bottom5 - 1)) * 3 in
    if map land two_bottom5 = 0 then
      { map   = map lor two_bottom5
      ; items =
          let l  = Array.length t in
          let t' = Array.make (l + 3) (Obj.repr 0) in
          unsafe_blit t 0 t' 0 offset;
          Array.unsafe_set t' (offset+1) (Obj.repr key);
          Array.unsafe_set t' (offset+2) (Obj.repr v);
          unsafe_blit t offset t' (offset+3) (l - offset);
          t'
      }
    else
(*      let t' = Array.copy t in*)
(*      let l  = Array.length t in
      let t' = Array.make l (Obj.repr 0) in
      unsafe_blit t 0 t' 0 l;*)
      let t' = t (*Obj.obj (Obj.dup (Obj.repr t))*) in
      let tag = Obj.obj (Array.unsafe_get t' offset) in
      if tag = 0 && Obj.obj (Array.unsafe_get t' (offset+1)) = key then
        Array.unsafe_set t' (offset+2) (Obj.repr v)
      else if tag = 0 then
        let key' = Obj.obj (Array.unsafe_get t' (offset+1)) in
        let v'   = Obj.obj (Array.unsafe_get t' (offset+2)) in
        set_subtree key (skey lsr 5) v key' (key' lsr (5*d)) v' t' offset
      else begin
        let submap = Obj.obj (Array.unsafe_get t' (offset+1)) in
        let subt   = Obj.obj (Array.unsafe_get t' (offset+2)) in
        let {map=submap;items=subt} =
          insertion (d+1) (skey lsr 5) submap subt in
        Array.unsafe_set t' (offset+1) (Obj.repr submap);
        Array.unsafe_set t' (offset+2) (Obj.repr subt)
      end;
      {map;items=t'}
  in
  insertion 1 key map items
