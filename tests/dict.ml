type key = string [@@deriving rpc]

type t = (key * float) list [@@deriving rpc]

let _ =
  let t = [ "foo", 3. ; "bar", 4. ] in
  let r = rpc_of t in
  Printf.printf "r = %s\n%!" (Rpc.to_string r);

  let t' = of_rpc r in
  Printf.printf "t = t' : %b\n%!" (t = t');
  assert (t = t')
