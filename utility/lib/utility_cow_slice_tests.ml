open Core

module Cow_slice = Utility_cow_slice
module Dynarray = Utility_dynarray

let%test_module "Utility_cow_slice" =
  (module struct
    let print_int_t t = print_s [%sexp (t : int Cow_slice.t)]
    let print_pair_t t = print_s [%sexp (t : (int * int) Cow_slice.t)]

    let print_error thunk =
      match Or_error.try_with thunk with
      | Ok () -> print_endline "Ok"
      | Error err -> print_endline (Error.to_string_hum err)
    ;;

    let%expect_test "constructors conversions and sexp" =
      let empty = Cow_slice.create 4 in
      print_s [%sexp ((Cow_slice.length empty, Cow_slice.is_empty empty) : int * bool)];
      print_int_t empty;
      let singleton = Cow_slice.singleton 7 in
      print_int_t singleton;
      let init = Cow_slice.init 4 ~f:(fun i -> i * i) in
      print_int_t init;
      print_s [%sexp (Cow_slice.to_list_rev init : int list)];
      print_s [%sexp (Cow_slice.to_array init : int array)];
      let from_array = Cow_slice.of_array [| 5; 6; 7 |] in
      let from_list = Cow_slice.of_list [ 8; 9 ] in
      print_int_t from_array;
      print_int_t from_list;
      let sexp = Cow_slice.sexp_of_t Int.sexp_of_t from_array in
      print_s sexp;
      print_int_t (Cow_slice.t_of_sexp Int.t_of_sexp sexp);
      [%expect {|
        (0 true)
        ()
        (7)
        (0 1 4 9)
        (9 4 1 0)
        (0 1 4 9)
        (5 6 7)
        (8 9)
        (5 6 7)
        (5 6 7)
      |}]
    ;;

    let%expect_test "copy push append and purity" =
      let base = Cow_slice.of_list [ 1; 2 ] in
      let alias = base in
      let copied = Cow_slice.copy base in
      let pushed = Cow_slice.push base 3 in
      print_int_t alias;
      print_int_t copied;
      print_int_t pushed;
      let partial = Cow_slice.slice base 0 1 in
      let partial_pushed = Cow_slice.push partial 9 in
      print_int_t base;
      print_int_t partial;
      print_int_t partial_pushed;
      let appended = Cow_slice.append base (Cow_slice.of_list [ 4; 5 ]) in
      let appended_alias = base in
      print_int_t appended_alias;
      print_int_t appended;
      let appended_partial = Cow_slice.append partial (Cow_slice.of_list [ 6; 7 ]) in
      print_int_t partial;
      print_int_t appended_partial;
      let appended_array = Cow_slice.append_array base [| 8; 9 |] in
      let appended_list = Cow_slice.append_list base [ 10; 11 ] in
      print_int_t appended_array;
      print_int_t appended_list;
      let full_append_array =
        Cow_slice.append_array_full_slice_exn (Cow_slice.of_list [ 1; 2 ]) [| 12 |]
      in
      let full_append_list =
        Cow_slice.append_list_full_slice_exn (Cow_slice.of_list [ 1; 2 ]) [ 13 ]
      in
      let full_append =
        Cow_slice.append_full_slice_exn (Cow_slice.of_list [ 1; 2 ]) (Cow_slice.of_list [ 14 ])
      in
      print_int_t full_append_array;
      print_int_t full_append_list;
      print_int_t full_append;
      print_error (fun () ->
        ignore (Cow_slice.push_full_slice_exn partial 0 : int Cow_slice.t));
      print_error (fun () ->
        ignore
          (Cow_slice.append_full_slice_exn partial (Cow_slice.of_list [ 1 ]) : int Cow_slice.t));
      print_error (fun () ->
        ignore (Cow_slice.append_array_full_slice_exn partial [| 1 |] : int Cow_slice.t));
      print_error (fun () ->
        ignore (Cow_slice.append_list_full_slice_exn partial [ 1 ] : int Cow_slice.t));
      let unchecked = Cow_slice.Unchecked.mutate base ~f:(fun a -> Dynarray.push a 15) in
      print_int_t base;
      print_int_t unchecked;
      print_error (fun () ->
        ignore
          (Cow_slice.Unchecked.mutate_full_slice_exn partial ~f:(fun a -> Dynarray.push a 1)
            : int Cow_slice.t));
      let unchecked_full =
        Cow_slice.Unchecked.mutate_full_slice_exn
          (Cow_slice.of_list [ 1; 2 ])
          ~f:(fun a -> Dynarray.push a 16)
      in
      print_int_t unchecked_full;
      print_int_t
        (Cow_slice.concat [ base; Cow_slice.of_list [ 20 ]; Cow_slice.of_list [ 21; 22 ] ]);
      [%expect {|
        (1 2)
        (1 2)
        (1 2 3)
        (1 2)
        (1)
        (1 9)
        (1 2)
        (1 2 4 5)
        (1)
        (1 6 7)
        (1 2 8 9)
        (1 2 10 11)
        (1 2 12)
        (1 2 13)
        (1 2 14)
        (Failure "mutate_full_slice_exn: array was mutated since last call")
        (Failure "mutate_full_slice_exn: array was mutated since last call")
        (Failure "mutate_full_slice_exn: array was mutated since last call")
        (Failure "mutate_full_slice_exn: array was mutated since last call")
        (1 2)
        (1 2 15)
        (Failure "mutate_full_slice_exn: array was mutated since last call")
        (1 2 16)
        (1 2 20 21 22)
      |}]
    ;;

    let%expect_test "access iteration and searches" =
      let t = Cow_slice.of_list [ 2; 4; 4; 8 ] in
      print_s
        [%sexp
          (( Cow_slice.get t 1
           , Cow_slice.last t
           , Cow_slice.slice t 1 3
           , Cow_slice.slice t 2 0 )
            : int * int * int Cow_slice.t * int Cow_slice.t)];
      let iter_seen = ref [] in
      Cow_slice.iter t ~f:(fun x -> iter_seen := x :: !iter_seen);
      print_s [%sexp (List.rev !iter_seen : int list)];
      let iteri_seen = ref [] in
      Cow_slice.iteri t ~f:(fun i x -> iteri_seen := (i, x) :: !iteri_seen);
      print_s [%sexp (List.rev !iteri_seen : (int * int) list)];
      print_s
        [%sexp
          (( Cow_slice.fold_left t ~init:0 ~f:( + )
           , Cow_slice.fold_right t ~init:0 ~f:( + )
           , Cow_slice.for_all t ~f:(fun x -> x mod 2 = 0)
           , Cow_slice.for_alli t ~f:(fun i x -> i < x)
           , Cow_slice.exists t ~f:(fun x -> x = 8)
           , Cow_slice.existsi t ~f:(fun i x -> i = x)
           , Cow_slice.mem t 4 ~equal:Int.equal )
            : int * int * bool * bool * bool * bool * bool)];
      print_s
        [%sexp
          (( Cow_slice.find t ~f:(fun x -> x > 4)
           , Cow_slice.findi t ~f:(fun i x -> i = 2 && x = 4)
           , Cow_slice.find_index t ~f:(fun x -> x = 8)
           , Cow_slice.find_map t ~f:(fun x -> if x = 4 then Some (x + 1) else None)
           , Cow_slice.find_mapi t ~f:(fun i x -> if i = 3 then Some (x + 10) else None)
           , Cow_slice.find_consecutive_duplicate t ~equal:Int.equal )
            : int option
              * (int * int) option
              * int option
              * int option
              * int option
              * (int * int) option)];
      print_s
        [%sexp
          (( Cow_slice.find_exn t ~f:(fun x -> x = 8)
           , Cow_slice.findi_exn t ~f:(fun i x -> i = 0 && x = 2)
           , Cow_slice.find_map_exn t ~f:(fun x -> if x = 8 then Some 99 else None)
           , Cow_slice.find_mapi_exn t ~f:(fun i x -> if i = 1 then Some (x * 10) else None)
           )
            : int * (int * int) * int * int)];
      print_error (fun () ->
        ignore (Cow_slice.find_exn t ~f:(fun x -> x = 7) : int));
      print_error (fun () ->
        ignore (Cow_slice.findi_exn t ~f:(fun _ x -> x = 7) : int * int));
      print_error (fun () ->
        ignore (Cow_slice.find_map_exn t ~f:(fun _ -> None) : int));
      print_error (fun () ->
        ignore (Cow_slice.find_mapi_exn t ~f:(fun _ _ -> None) : int));
      [%expect {|
        (4 8 (4 4) (4 8))
        (2 4 4 8)
        ((0 2) (1 4) (2 4) (3 8))
        (18 18 true true true false true)
        ((8) ((2 4)) (3) (5) (18) ((4 4)))
        (8 (0 2) 99 40)
        (Not_found_s "Cow_slice.find_exn: not found")
        (Not_found_s "Cow_slice.findi_exn: not found")
        (Not_found_s "Cow_slice.find_map_exn: not found")
        (Not_found_s "Cow_slice.find_mapi_exn: not found")
      |}]
    ;;

    let%expect_test "transforms reducers ordering and comparison" =
      let t = Cow_slice.of_list [ 1; 2; 3; 4 ] in
      print_int_t (Cow_slice.map t ~f:(fun x -> x * 2));
      print_int_t (Cow_slice.mapi t ~f:(fun i x -> i + x));
      let acc, mapped = Cow_slice.fold_map t ~init:0 ~f:(fun acc x -> acc + x, x + 10) in
      print_s [%sexp ((acc, mapped) : int * int Cow_slice.t)];
      let acc, mapped =
        Cow_slice.fold_mapi t ~init:100 ~f:(fun i acc x -> acc - x, i + x)
      in
      print_s [%sexp ((acc, mapped) : int * int Cow_slice.t)];
      print_int_t (Cow_slice.folding_map t ~init:0 ~f:(fun acc x -> acc + x, acc + x));
      print_int_t
        (Cow_slice.folding_mapi t ~init:0 ~f:(fun i acc x -> acc + x, i + acc + x));
      print_int_t (Cow_slice.filter t ~f:(fun x -> x mod 2 = 0));
      print_int_t (Cow_slice.filteri t ~f:(fun i _ -> i mod 2 = 1));
      print_int_t (Cow_slice.filter_map t ~f:(fun x -> if x mod 2 = 0 then Some (x * 3) else None));
      print_int_t
        (Cow_slice.filter_mapi t ~f:(fun i x -> if i < 2 then Some (x + i) else None));
      print_int_t (Cow_slice.filter_opt (Cow_slice.of_list [ Some 1; None; Some 3 ]));
      let yes, no = Cow_slice.partition_tf t ~f:(fun x -> x <= 2) in
      print_s [%sexp ((yes, no) : int Cow_slice.t * int Cow_slice.t)];
      let yes, no = Cow_slice.partitioni_tf t ~f:(fun i _ -> i mod 2 = 0) in
      print_s [%sexp ((yes, no) : int Cow_slice.t * int Cow_slice.t)];
      print_int_t (Cow_slice.rev t);
      print_s
        [%sexp
          (( Cow_slice.reduce t ~f:( + )
           , Cow_slice.reduce_exn t ~f:Int.max
           , Cow_slice.equal t (Cow_slice.of_array [| 1; 2; 3; 4 |]) ~equal:Int.equal
           , Cow_slice.compare t (Cow_slice.of_list [ 1; 2; 3; 5 ]) ~compare:Int.compare
           , Cow_slice.is_sorted t ~compare:Int.compare
           , Cow_slice.is_sorted_strictly t ~compare:Int.compare
           , Cow_slice.is_sorted (Cow_slice.of_list [ 1; 1; 2 ]) ~compare:Int.compare
           , Cow_slice.is_sorted_strictly (Cow_slice.of_list [ 1; 1; 2 ]) ~compare:Int.compare
           )
            : int option * int * bool * int * bool * bool * bool * bool)];
      print_s [%sexp (Cow_slice.reduce (Cow_slice.create 0) ~f:( + ) : int option)];
      print_error (fun () ->
        ignore (Cow_slice.reduce_exn (Cow_slice.create 0) ~f:( + ) : int));
      [%expect {|
        (2 4 6 8)
        (1 3 5 7)
        (10 (11 12 13 14))
        (90 (1 3 5 7))
        (1 3 6 10)
        (1 4 8 13)
        (2 4)
        (2 4)
        (6 12)
        (1 3)
        (1 3)
        ((1 2) (3 4))
        ((1 3) (2 4))
        (4 3 2 1)
        ((10) 4 true -1 true true true false)
        ()
        (Failure "Cow_slice.reduce_exn: empty slice")
      |}]
    ;;

    let%expect_test "pairwise operations" =
      let t1 = Cow_slice.of_list [ 1; 2; 3 ] in
      let t2 = Cow_slice.of_list [ 10; 20; 30 ] in
      let iter2_seen = ref [] in
      Cow_slice.iter2_exn t1 t2 ~f:(fun x y -> iter2_seen := (x, y) :: !iter2_seen);
      print_s [%sexp (List.rev !iter2_seen : (int * int) list)];
      print_int_t (Cow_slice.map2_exn t1 t2 ~f:( + ));
      print_s [%sexp (Cow_slice.fold2_exn t1 t2 ~init:0 ~f:(fun acc x y -> acc + x + y) : int)];
      print_s
        [%sexp
          (( Cow_slice.for_all2_exn t1 t2 ~f:(fun x y -> y = x * 10)
           , Cow_slice.exists2_exn t1 t2 ~f:(fun x y -> x + y = 22) )
            : bool * bool)];
      print_s [%sexp (Cow_slice.zip t1 t2 : (int * int) Cow_slice.t option)];
      print_pair_t (Cow_slice.zip_exn t1 t2);
      let left, right = Cow_slice.unzip (Cow_slice.zip_exn t1 t2) in
      print_s [%sexp ((left, right) : int Cow_slice.t * int Cow_slice.t)];
      let short = Cow_slice.of_list [ 1; 2 ] in
      print_s [%sexp (Cow_slice.zip t1 short : (int * int) Cow_slice.t option)];
      print_error (fun () -> Cow_slice.iter2_exn t1 short ~f:(fun _ _ -> ()));
      print_error (fun () -> ignore (Cow_slice.map2_exn t1 short ~f:( + ) : int Cow_slice.t));
      print_error (fun () ->
        ignore (Cow_slice.fold2_exn t1 short ~init:0 ~f:(fun acc x y -> acc + x + y) : int));
      print_error (fun () ->
        ignore (Cow_slice.for_all2_exn t1 short ~f:(fun _ _ -> true) : bool));
      print_error (fun () ->
        ignore (Cow_slice.exists2_exn t1 short ~f:(fun _ _ -> true) : bool));
      print_error (fun () -> ignore (Cow_slice.zip_exn t1 short : (int * int) Cow_slice.t));
      [%expect {|
        ((1 10) (2 20) (3 30))
        (11 22 33)
        66
        (true true)
        (((1 10) (2 20) (3 30)))
        ((1 10) (2 20) (3 30))
        ((1 2 3) (10 20 30))
        ()
        (Failure "Cow_slice.iter2_exn: length mismatch. 3 <> 2")
        (Failure "Cow_slice.map2_exn: length mismatch. 3 <> 2")
        (Failure "Cow_slice.fold2_exn: length mismatch. 3 <> 2")
        (Failure "Cow_slice.for_all2_exn: length mismatch. 3 <> 2")
        (Failure "Cow_slice.exists2_exn: length mismatch. 3 <> 2")
        (Failure "Cow_slice.zip_exn: length mismatch. 3 <> 2")
      |}]
    ;;
  end)
