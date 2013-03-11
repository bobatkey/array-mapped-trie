open Benchmark

let size = 50000

let data_list =
  let rec loop acc n =
    if n = 0 then acc
    else loop (Random.int 1000000 - 500000::acc) (n-1)
  in
  loop [] size

(******************************************************************************)
module MapMakeInt = Map.Make
  (struct type t = int
          let compare (x:int) (y:int) = Pervasives.compare x y end)

module H = Hashtbl.Make (struct type t = int
                                let equal (x:int) y = x = y
                                let hash x = x land 0x3fffffff end)

(******************************************************************************)
let mapmakeint_inserts () =
  ignore (List.fold_left
            (fun m i -> MapMakeInt.add i () m)
            MapMakeInt.empty
            data_list)

let intmap_inserts () =
  ignore (List.fold_left
            (fun m i -> IntMap.BigEndian.add i () m)
            IntMap.BigEndian.empty
            data_list)

let intmap_le_inserts () =
  ignore (List.fold_left
            (fun m i -> IntMap.LittleEndian.add i () m)
            IntMap.LittleEndian.empty
            data_list)

let hashtbl_inserts () =
  let h = H.create 1000 in
  List.iter (fun i -> H.replace h i ()) data_list

let hashtbl2_inserts () =
  let h = H.create 100000 in
  List.iter (fun i -> H.replace h i ()) data_list

let amt_inserts () =
  ignore (List.fold_left
            (fun m i -> ArrayMappedTrie.add i () m)
            ArrayMappedTrie.empty
            data_list)

let amt_packed_inserts () =
  let t = ArrayMappedTrie_packed.create () in
  List.iter (fun i -> ArrayMappedTrie_packed.add i () t) data_list

(******************************************************************************)
let mapmakeint_inserts_lookups () =
  let t =
    List.fold_left
      (fun m i -> MapMakeInt.add i () m)
      MapMakeInt.empty
      data_list
  in
  List.iter (fun i -> MapMakeInt.find i t) data_list

let intmap_inserts_lookups () =
  let t =
    List.fold_left
      (fun m i -> IntMap.BigEndian.add i () m)
      IntMap.BigEndian.empty
      data_list
  in
  List.iter (fun i -> IntMap.BigEndian.find i t) data_list

let intmap_le_inserts_lookups () =
  let t =
    List.fold_left
      (fun m i -> IntMap.LittleEndian.add i () m)
      IntMap.LittleEndian.empty
      data_list
  in
  List.iter (fun i -> IntMap.LittleEndian.find i t) data_list

let hashtbl_inserts_lookups () =
  let h = H.create 1000 in
  List.iter (fun i -> H.replace h i ()) data_list;
  List.iter (fun i -> H.find h i) data_list

let hashtbl2_inserts_lookups () =
  let h = H.create 100000 in
  List.iter (fun i -> H.replace h i ()) data_list;
  List.iter (fun i -> H.find h i) data_list

let amt_inserts_lookups () =
  let t =
    List.fold_left (fun m i -> ArrayMappedTrie.add i () m)
      ArrayMappedTrie.empty
      data_list
  in
  List.iter (fun i -> ArrayMappedTrie.find i t) data_list

let amt_packed_inserts_lookups () =
  let t = ArrayMappedTrie_packed.create () in
  List.iter (fun i -> ArrayMappedTrie_packed.add i () t) data_list;
  List.iter (fun i -> ArrayMappedTrie_packed.find i t) data_list

(******************************************************************************)
let mapmakeint_t =
  List.fold_left
    (fun m i -> MapMakeInt.add i () m)
    MapMakeInt.empty
    data_list

let mapmakeint_lookups () =
  List.iter (fun i -> MapMakeInt.find i mapmakeint_t) data_list


let intmap_t =
  List.fold_left
    (fun m i -> IntMap.BigEndian.add i () m)
    IntMap.BigEndian.empty
    data_list

let intmap_lookups () =
  List.iter (fun i -> IntMap.BigEndian.find i intmap_t) data_list


let intmap_le_t =
  List.fold_left
    (fun m i -> IntMap.LittleEndian.add i () m)
    IntMap.LittleEndian.empty
    data_list

let intmap_le_lookups () =
  List.iter (fun i -> IntMap.LittleEndian.find i intmap_le_t) data_list


let hashtbl_t =
  let h = H.create 1000 in
  List.iter (fun i -> H.replace h i ()) data_list;
  h

let hashtbl_lookups () =
  List.iter (fun i -> H.find hashtbl_t i) data_list


let hashtbl2_t =
  let h = H.create 100000 in
  List.iter (fun i -> H.replace h i ()) data_list;
  h

let hashtbl2_lookups () =
  List.iter (fun i -> H.find hashtbl2_t i) data_list


let amt_t =
  List.fold_left (fun m i -> ArrayMappedTrie.add i () m)
    ArrayMappedTrie.empty
    data_list

let amt_lookups () =
  List.iter (fun i -> ArrayMappedTrie.find i amt_t) data_list


let amt_packed_t =
  let t = ArrayMappedTrie_packed.create () in
  List.iter (fun i -> ArrayMappedTrie_packed.add i () t) data_list;
  t

let amt_packed_lookups () =
  List.iter (fun i -> ArrayMappedTrie_packed.find i amt_packed_t) data_list

(******************************************************************************)
let benchmark_add () =
  let res =
    latencyN 500L
      [ "arraymappedtrie", amt_inserts, ()
      ; "arraymappedtrie_packed", amt_packed_inserts, ()
      ; "intmap", intmap_inserts, ()
      ; "intmap_le", intmap_le_inserts, ()
      ; "mapmakeint", mapmakeint_inserts, ()
      ; "hashtbl", hashtbl_inserts, ()
      ; "hashtbl2", hashtbl2_inserts, ()
      ]
  in
  print_newline ();
  tabulate res

let benchmark_add_find () =
  let res =
    latencyN 500L
      [ "arraymappedtrie", amt_inserts_lookups, ()
      ; "arraymappedtrie_packed", amt_packed_inserts_lookups, ()
(*      ; "intmap", intmap_inserts_lookups, ()
      ; "intmap_le", intmap_le_inserts_lookups, ()
      ; "mapmakeint", mapmakeint_inserts_lookups, () *)
      ; "hashtbl",  hashtbl_inserts_lookups,  ()
      ; "hashtbl2", hashtbl2_inserts_lookups, ()
      ]
  in
  print_newline ();
  tabulate res

let benchmark_find () =
  let res =
    latencyN 500L
      [ "arraymappedtrie", amt_lookups, ()
      ; "arraymappedtrie_packed", amt_packed_lookups, ()
(*      ; "intmap", intmap_lookups, ()
      ; "intmap_le", intmap_le_lookups, ()
      ; "mapmakeint", mapmakeint_lookups, () *)
      ; "hashtbl",  hashtbl_lookups,  ()
      ; "hashtbl2", hashtbl2_lookups, ()
      ]
  in
  print_newline ();
  tabulate res

let _ =
  Random.init 12345;
(*  Gc.set { (Gc.get ()) with Gc.minor_heap_size = 262144 * 20 };*)
  benchmark_add ();
  print_newline ();
  benchmark_add_find ();
  print_newline ();
  benchmark_find ()
