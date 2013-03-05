open Benchmark

(* do a benchmark that inserts the numbers 1 to 100000 into
   - an IntMap
   - a ArrayMappedTrie
   - a Hashtbl
*)

module MapMakeInt = Map.Make
  (struct type t = int
          let compare (x:int) (y:int) = Pervasives.compare x y end)

module H = Hashtbl.Make (struct type t = int
                                let equal (x:int) y = x = y
                                let hash x = x land 0xffffffff end)

(******************************************************************************)
let mapmakeint_inserts n =
  let rec loop map i =
    if i = 0 then map
    else loop (MapMakeInt.add i () map) (i-1)
  in
  ignore (loop MapMakeInt.empty n)

let intmap_inserts n =
  let rec loop map i =
    if i = 0 then map
    else loop (IntMap.BigEndian.add i () map) (i-1)
  in
  ignore (loop IntMap.BigEndian.empty n)

let intmap_le_inserts n =
  let rec loop map i =
    if i = 0 then map
    else loop (IntMap.LittleEndian.add i () map) (i-1)
  in
  ignore (loop IntMap.LittleEndian.empty n)

let hashtbl_inserts n =
  let h = H.create 1000 in
  let rec loop i =
    if i = 0 then ()
    else (H.replace h i (); loop (i-1))
  in
  loop n

let hashtbl2_inserts n =
  let h = H.create 100000 in
  let rec loop i =
    if i = 0 then ()
    else (H.replace h i (); loop (i-1))
  in
  loop n

let amt_inserts n =
  let rec loop tree i =
    if i = 0 then tree
    else loop (ArrayMappedTrie.add i () tree) (i-1)
  in
  ignore (loop ArrayMappedTrie.empty n)

let amt_packed_inserts n =
  let rec loop tree i =
    if i = 0 then tree
    else (ArrayMappedTrie_packed.add i () tree; loop tree (i-1))
  in
  ignore (loop (ArrayMappedTrie_packed.create ()) n)

(******************************************************************************)
let mapmakeint_inserts_lookups n =
  let rec loop map i =
    if i = 0 then map
    else loop (MapMakeInt.add i () map) (i-1)
  in
  let rec loop2 map i =
    if i = 0 then ()
    else
      let () = MapMakeInt.find i map in
      loop2 map (i-1)
  in
  ignore (loop2 (loop MapMakeInt.empty n) n)

let intmap_inserts_lookups n =
  let rec loop map i =
    if i = 0 then map
    else loop (IntMap.BigEndian.add i () map) (i-1)
  in
  let rec loop2 map i =
    if i = 0 then ()
    else
      let () = IntMap.BigEndian.find i map in
      loop2 map (i-1)
  in
  ignore (loop2 (loop IntMap.BigEndian.empty n) n)

let intmap_le_inserts_lookups n =
  let rec loop map i =
    if i = 0 then map
    else loop (IntMap.LittleEndian.add i () map) (i-1)
  in
  let rec loop2 map i =
    if i = 0 then ()
    else
      let () = IntMap.LittleEndian.find i map in
      loop2 map (i-1)
  in
  ignore (loop2 (loop IntMap.LittleEndian.empty n) n)

let hashtbl_inserts_lookups n =
  let h = H.create 1000 in
  let rec loop i =
    if i = 0 then ()
    else (H.replace h i (); loop (i-1))
  in
  let rec loop2 i =
    if i = 0 then ()
    else (H.find h i; loop2 (i-1))
  in
  loop n; loop2 n

let hashtbl2_inserts_lookups n =
  let h = H.create 100000 in
  let rec loop i =
    if i = 0 then ()
    else (H.replace h i (); loop (i-1))
  in
  let rec loop2 i =
    if i = 0 then ()
    else (H.find h i; loop2 (i-1))
  in
  loop n; loop2 n

let amt_inserts_lookups n =
  let rec loop tree i =
    if i = 0 then tree
    else loop (ArrayMappedTrie.add i () tree) (i-1)
  in
  let rec loop2 tree i =
    if i = 0 then ()
    else let _ = ArrayMappedTrie.find i tree in loop2 tree (i-1)
  in
  loop2 (loop ArrayMappedTrie.empty n) n

let amt_packed_inserts_lookups n =
  let rec loop tree i =
    if i = 0 then tree
    else (ArrayMappedTrie_packed.add i () tree; loop tree (i-1))
  in
  let rec loop2 tree i =
    if i = 0 then ()
    else let _ = ArrayMappedTrie_packed.find i tree in loop2 tree (i-1)
  in
  loop2 (loop (ArrayMappedTrie_packed.create ()) n) n


(******************************************************************************)
let benchmark1 () =
  let res =
    latencyN 500L
      [ "arraymappedtrie", amt_inserts, 100000
      ; "arraymappedtrie_packed", amt_packed_inserts, 100000
      ; "intmap",   intmap_inserts,   100000
      ; "intmap_le",   intmap_le_inserts,   100000
      ; "mapmakeint",   mapmakeint_inserts,   100000
      ; "hashtbl",  hashtbl_inserts,  100000
      ; "hashtbl2", hashtbl2_inserts, 100000
      ]
  in
  print_newline ();
  tabulate res

let benchmark2 () =
  let res =
    latencyN 500L
      [ "arraymappedtrie", amt_inserts_lookups, 100000
      ; "arraymappedtrie_packed", amt_packed_inserts_lookups, 100000
      ; "intmap",   intmap_inserts_lookups,   100000
      ; "intmap_le",   intmap_le_inserts_lookups,   100000
      ; "mapmakeint",   mapmakeint_inserts_lookups,   100000
      ; "hashtbl",  hashtbl_inserts_lookups,  100000
      ; "hashtbl2", hashtbl2_inserts_lookups, 100000
      ]
  in
  print_newline ();
  tabulate res

let _ =
(*  Gc.set { (Gc.get ()) with Gc.minor_heap_size = 262144 * 20 };*)
  benchmark1 ();
  print_newline ();
  benchmark2 ()
