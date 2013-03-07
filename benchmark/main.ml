open Benchmark

(* do a benchmark that inserts the numbers 1 to 100000 into
   - an IntMap
   - a ArrayMappedTrie
   - a Hashtbl
*)

let size = 50000

let data_list =
  let rec loop acc n =
    if n = 0 then acc
    else loop (Random.bits ()::acc) (n-1)
  in
  loop [] size

(******************************************************************************)
module MapMakeInt = Map.Make
  (struct type t = int
          let compare (x:int) (y:int) = Pervasives.compare x y end)

module H = Hashtbl.Make (struct type t = int
                                let equal (x:int) y = x = y
                                let hash x = x land 0xffffffff end)

(******************************************************************************)
let mapmakeint_inserts n =
  ignore (List.fold_left (fun m n -> MapMakeInt.add n () m) MapMakeInt.empty data_list)

let intmap_inserts n =
  ignore (List.fold_left (fun m n -> IntMap.BigEndian.add n () m) IntMap.BigEndian.empty data_list)

let intmap_le_inserts n =
  ignore (List.fold_left (fun m n -> IntMap.LittleEndian.add n () m) IntMap.LittleEndian.empty data_list)

let hashtbl_inserts n =
  let h = H.create 1000 in
  List.iter (fun i -> H.replace h i ()) data_list

let hashtbl2_inserts n =
  let h = H.create 100000 in
  List.iter (fun i -> H.replace h i ()) data_list

let amt_inserts n =
  ignore (List.fold_left (fun m n -> ArrayMappedTrie.add n () m) ArrayMappedTrie.empty data_list)

let amt_packed_inserts n =
  let t = ArrayMappedTrie_packed.create () in
  List.iter (fun i -> ArrayMappedTrie_packed.add i () t) data_list

(******************************************************************************)
let mapmakeint_inserts_lookups n =
  let t = List.fold_left (fun m n -> MapMakeInt.add n () m) MapMakeInt.empty data_list in
  List.iter (fun i -> MapMakeInt.find i t) data_list

let intmap_inserts_lookups n =
  let t = List.fold_left (fun m n -> IntMap.BigEndian.add n () m) IntMap.BigEndian.empty data_list in
  List.iter (fun i -> IntMap.BigEndian.find i t) data_list

let intmap_le_inserts_lookups n =
  let t = List.fold_left (fun m n -> IntMap.LittleEndian.add n () m) IntMap.LittleEndian.empty data_list in
  List.iter (fun i -> IntMap.LittleEndian.find i t) data_list

let hashtbl_inserts_lookups n =
  let h = H.create 1000 in
  List.iter (fun i -> H.replace h i ()) data_list;
  List.iter (fun i -> H.find h i) data_list

let hashtbl2_inserts_lookups n =
  let h = H.create 100000 in
  List.iter (fun i -> H.replace h i ()) data_list;
  List.iter (fun i -> H.find h i) data_list

let amt_inserts_lookups n =
  let t = List.fold_left (fun m n -> ArrayMappedTrie.add n () m) ArrayMappedTrie.empty data_list in
  List.iter (fun i -> ArrayMappedTrie.find i t) data_list

let amt_packed_inserts_lookups n =
  let t = ArrayMappedTrie_packed.create () in
  List.iter (fun i -> ArrayMappedTrie_packed.add i () t) data_list;
  List.iter (fun i -> ArrayMappedTrie_packed.find i t) data_list

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
  Random.init 12345;
(*  Gc.set { (Gc.get ()) with Gc.minor_heap_size = 262144 * 20 };*)
  benchmark1 ();
  print_newline ();
  benchmark2 ()
