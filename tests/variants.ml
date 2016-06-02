type t = [ `foo | `bar of int * string | `baz of char] [@@deriving rpc]

let _ =
  let open Rpc in
  let t1 = `foo in
  let t2 = `bar (3, "bar") in
  let t3 = `baz 'c' in
  
  let r1 = rpc_of t1 in
  let r2 = rpc_of t2 in
  let r3 = rpc_of t3 in

  Printf.printf "r1 = %s\nr2 = %s\nr3 = %s\n%!" (Rpc.to_string r1) (Rpc.to_string r2) (Rpc.to_string r3);

  of_rpc r1 >>= fun t1' ->
  of_rpc r2 >>= fun t2' ->
  of_rpc r3 >>= fun t3' ->

  Printf.printf "t1 = t1' : %b\nt2 = t2' : %b\nt3 = t3' : %b\n%!" (t1 = t1') (t2 = t2') (t3 = t3');
  assert (t1 = t1' && t2 = t2' && t3 = t3');

  let test3 = Rpc.String "FOO" in
  match (of_rpc test3) with
  | Ok _ -> 
    Printf.printf "Case insensitive test: OK\n!";
    return ()
  | Error _ ->
    Printf.printf "Case insensitive test failed!\n%!";
    exit 1

	  
