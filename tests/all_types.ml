(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

type t =
  | Foo of int
  | Bar of (int * float)
  | Baz
  | Boo of int option
  [@@deriving rpc]

module M = struct
  type m = t [@@deriving rpc]
end

type 'a x = {
  foo: M.m;
  bar: string;
  gna: float list;
  f1: (int option * bool list * float list list) option;
  f2: (string * string list) array;
  f3: int32;
  f4: int64;
  f5: int [@key "type"];
  f6: (unit * char) list;
  f7: 'a list [@key "let"];
  f8: t list;
  f9: int option;
  progress: int array;
} [@@deriving rpc]

let file_of_string fname s =
  let oc = open_out fname in
  Printf.fprintf oc "%s" s;
  close_out oc

let file_exists f =
  try ignore (Unix.stat f); true
  with _ -> false

let string_of_file fname =
  let ic = open_in fname in
  let rec read so_far =
    try
      let line = input_line ic in
      read (line::so_far)
    with End_of_file ->
      close_in ic;
      String.concat "" (List.rev so_far)
  in
  read [] 


let _ =
  let x = {
    foo= Foo 3;
    bar= "ha          ha";
    gna=[1.; 2.; 3.; 4.; 1464103984.0 ];
    f2 = [| "hi",["hi"]; "hou",["hou";"hou"]; "foo", ["b";"a";"r"] |];
    f1 = Some (None, [true], [[1.]; [2.;3.]]);
    f3 = Int32.max_int;
    f4 = Int64.max_int;
    f5 = max_int;
    f6 = [ (),'a' ; (),'b' ; (),'c'; (),'d' ; (),'e' ];
    f7 = [ Foo 1; Foo 2; Foo 3 ];
    f8 = [ Foo 1; Bar (2, 2.0); Baz; Boo (Some 1); Boo None];
    f9 = None;
    progress = [| 0; 1; 2; 3; 4; 5 |];
  } in

  (* Testing basic marshalling/unmarshalling *)

  let rpc = rpc_of_x M.rpc_of_m x in

  let rpc_xml = Xmlrpc.to_string rpc in
  let rpc_json = Jsonrpc.to_string rpc in

  file_of_string "x.xml" rpc_xml;
  file_of_string "x.json" rpc_json;
      
  Printf.printf "\n==rpc_xml==\n%s\n" rpc_xml;
  Printf.printf "\n==json==\n%s\n" rpc_json;

  let callback fields value =
    match (fields, value) with
    | ["progress"], Rpc.Int i -> Printf.printf "Progress: %Ld\n" i
    | _                       -> () in

  let x_of_rpc = x_of_rpc M.m_of_rpc in

  let x_xml = x_of_rpc (Xmlrpc.of_string ~callback rpc_xml) in
  let x_json = x_of_rpc (Jsonrpc.of_string rpc_json) in

  Printf.printf "\n==Sanity check 1==\nx=x_xml: %b\nx=x_json: %b\n" (x = x_xml) (x = x_json);
  assert (x = x_xml && x = x_json);

  if file_exists "x.reference.xml" && file_exists "x.reference.json" then begin
    Printf.printf "\n==Backwards compatibility check==\n";
    let x_old_xml = string_of_file "x.reference.xml" |> Xmlrpc.of_string |> x_of_rpc in
    let x_old_json = string_of_file "x.reference.json" |> Jsonrpc.of_string |> x_of_rpc in
    assert (x=x_old_xml && x=x_old_json);
  end;
  
  (* Testing calls and responses *)

  let call = Rpc.call "foo" [ rpc ] in
  let success = Rpc.success rpc in
  let failure = Rpc.failure rpc in

  let c_xml_str = Xmlrpc.string_of_call call in
  let s_xml_str = Xmlrpc.string_of_response success in
  let f_xml_str = Xmlrpc.string_of_response failure in

  let c_json_str = Jsonrpc.string_of_call call in
  let s_json_str = Jsonrpc.string_of_response success in
  let f_json_str = Jsonrpc.string_of_response failure in

  Printf.printf "\n==call==\n %s\n%s\n" c_xml_str c_json_str;
  Printf.printf "\n==success==\n %s\n%s\n" s_xml_str s_json_str;
  Printf.printf "\n==failure==\n %s\n%s\n" f_xml_str f_json_str;

  let c_xml = Xmlrpc.call_of_string c_xml_str in
  let s_xml = Xmlrpc.response_of_string s_xml_str in
  let f_xml = Xmlrpc.response_of_string f_xml_str in

  let c1 = x_of_rpc (List.hd call.Rpc.params) in
  let c2 = x_of_rpc (List.hd c_xml.Rpc.params) in
  let s1 = x_of_rpc success.Rpc.contents in
  let s2 = x_of_rpc s_xml.Rpc.contents in
  let f1 = x_of_rpc failure.Rpc.contents in
  let f2 = x_of_rpc f_xml.Rpc.contents in

  Printf.printf "\n==Sanity check 2==\nc1=c2: %b\ns1=s2: %b\nf1=f2: %b\n"
    (c1 = c2) (s1 = s2) (f1 = f2);
  assert (c1 = c2 && s1 = s2 && f1 = f2);

  let c_json = Jsonrpc.call_of_string c_json_str in
  let s_json = Jsonrpc.response_of_string s_json_str in
  let f_json = Jsonrpc.response_of_string f_json_str in

  Printf.printf "\n==Sanity check 3==\ncall=c_json': %b\nsuccess=s_json': %b\nfailure=f_json': %b\n"
    (call = c_json) (success = s_json) (failure = f_json);
  assert (call = c_json && success = s_json && failure = f_json)
