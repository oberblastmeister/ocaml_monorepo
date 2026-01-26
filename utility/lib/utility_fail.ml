open Core

type t = Env

let env = Env

exception Fail

let fail (_env : t) = raise_notrace Fail

let unwrap env o =
  match o with
  | None -> fail env
  | Some x -> x
;;

let optional f =
  match f () with
  | exception Fail -> None
  | x -> Some x
;;

let either f g =
  match f () with
  | exception Fail -> First (g ())
  | x -> Second x
;;

let many_rev f =
  let rec loop acc =
    match f () with
    | exception Fail -> acc
    | x -> loop (x :: acc)
  in
  loop []
;;

let guard env b = if not b then fail env
let many f = many_rev f |> List.rev

let some_rev f =
  let x = f () in
  let xs = many_rev f in
  x :: xs
;;

let some f = some_rev f |> List.rev

let rec one_of fs =
  match fs with
  | [] -> raise_notrace Fail
  | f :: rest ->
    (match f () with
     | exception Fail -> one_of rest
     | x -> x)
;;

let cannot_fail ~f =
  match f () with
  | exception Fail -> failwith "Expected computation not to fail"
  | x -> x
;;

module List = struct
  type 'a t = 'a list ref

  let next env t =
    match !t with
    | [] -> fail env
    | x :: xs ->
      t := xs;
      x
  ;;

  let take t =
    let r = !t in
    t := [];
    r
  ;;

  let empty env t =
    match !t with
    | [] -> ()
    | _ :: _ -> fail env
  ;;

  let create env xs ~f =
    let r = ref xs in
    let x = f r in
    empty env r;
    x
  ;;

  let optional t f =
    let saved = !t in
    match f () with
    | exception Fail ->
      t := saved;
      None
    | x -> Some x
  ;;

  let either t f g =
    let saved = !t in
    match f () with
    | exception Fail ->
      t := saved;
      First (g ())
    | x -> Second x
  ;;

  let many_rev t f =
    let rec loop acc =
      let saved = !t in
      match f () with
      | exception Fail ->
        t := saved;
        acc
      | x -> loop (x :: acc)
    in
    loop []
  ;;

  let guard env _t b = if not b then fail env
  let many t f = many_rev t f |> List.rev

  let some_rev t f =
    let x = f () in
    let xs = many_rev t f in
    x :: xs
  ;;

  let some t f = some_rev t f |> List.rev

  let rec one_of t fs =
    match fs with
    | [] -> raise_notrace Fail
    | f :: rest ->
      let saved = !t in
      (match f () with
       | exception Fail ->
         t := saved;
         one_of t rest
       | x -> x)
  ;;
end

let run ~f =
  match f Env with
  | exception Fail -> None
  | x -> Some x
;;

let run_exn ~f =
  match f Env with
  | exception Fail -> failwith "Expected computation not to fail"
  | x -> x
;;

module Syntax = struct
  let ( <|> ) f g =
    match f () with
    | exception Fail -> g ()
    | x -> x
  ;;
end

let%test_module "List tests" =
  (module struct
    let%expect_test "many consumes all" =
      let t = ref [ 1; 2; 3 ] in
      let res = List.many t (fun () -> List.next Env t) in
      print_s [%sexp (res : int list)];
      [%expect {| (1 2 3) |}]
    ;;

    let%expect_test "optional backtracks" =
      let t = ref [ 1; 2 ] in
      let res =
        List.optional t (fun () ->
          let _ = List.next Env t in
          fail Env)
      in
      print_s [%sexp (res : int option)];
      print_s [%sexp (!t : int list)];
      [%expect
        {|
      ()
      (1 2)
    |}]
    ;;

    let%expect_test "either backtracks" =
      let t = ref [ 1; 2 ] in
      let res =
        List.either
          t
          (fun () ->
             let _ = List.next Env t in
             fail Env)
          (fun () -> List.next Env t)
      in
      (match res with
       | First x ->
         print_endline "First";
         print_s [%sexp (x : int)]
       | Second x ->
         print_endline "Second";
         print_s [%sexp (x : int)]);
      print_s [%sexp (!t : int list)];
      [%expect
        {|
      First
      1
      (2)
    |}]
    ;;
  end)
;;
