type t = string [@@deriving rpc]

let _ =
	let open Rpc in
	let t = "<provision><value><int>4</int></value>" in
	let r = rpc_of t in
	Printf.printf "r = %s\n%!" (Rpc.to_string r);

	of_rpc r >>= fun t' -> 
	Printf.printf "t = t : %b'\n%!" (t = t');
	assert (t = t');

	let test = Rpc.Dict ["foo", String "&"] in
	let str = Xmlrpc.to_string test in
	assert (str = "<value><struct><member><name>foo</name><value>&amp;</value></member></struct></value>");
	return ()

