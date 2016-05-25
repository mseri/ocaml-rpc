open Longident
open Asttypes
open Parsetree
open Location
open Ast_helper
open Ast_convenience
    
let deriver = "rpc"

let argn = Printf.sprintf "a%d"

let core_types = List.map (fun s -> Lident s) ["unit"; "int"; "int32"; "int64"; "string"; "float"; "bool"]

let is_option typ =
  match typ with
  | [%type: [%t? typ] option] -> true
  | _ -> false

let attr_string name default attrs =
  match Ppx_deriving.attr ~deriver name attrs |>
        Ppx_deriving.Arg.(get_attr ~deriver string) with
  | Some x -> x
  | None   -> default

let attr_key  = attr_string "key"

module Of_rpc = struct
  
  let rec expr_of_typ quoter typ =
    match typ with
    | { ptyp_desc = Ptyp_constr ( { txt = lid }, args ) } when
        List.mem lid core_types ->
      [%expr Rpc.([%e Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Suffix "of_rpc") lid))])]
    | { ptyp_desc = Ptyp_constr ( { txt = Lident "char" }, args ) } ->
      [%expr Rpc.(function | Rpc.Int x -> Char.chr (Int64.to_int x) | Rpc.String s -> Char.chr (int_of_string s))]
    | [%type: [%t? typ] list] -> [%expr function | Rpc.Enum l -> List.map [%e expr_of_typ quoter typ] l | _ -> failwith "boo" ]
    | [%type: [%t? typ] array] -> [%expr function | Rpc.Enum l -> Array.of_list (List.map [%e expr_of_typ quoter typ] l) | _ -> failwith "boo" ]
    | {ptyp_desc = Ptyp_tuple typs } ->
      let pattern = List.mapi (fun i _ -> pvar (argn i)) typs in
      [%expr fun (Rpc.Enum [%p plist pattern]) -> [%e tuple (List.mapi (fun i typ ->
                    let e = expr_of_typ quoter typ in
                    [%expr [%e e] [%e Exp.ident (lid (argn i))]]) typs)]]
    | [%type: [%t? typ] option] ->
      let e = expr_of_typ quoter typ in
      [%expr fun x -> match x with Rpc.Enum [] -> None | Rpc.Enum [y] -> Some ([%e e] y) ]
    | { ptyp_desc = Ptyp_constr ( { txt = lid }, args ) } ->
      let args = List.map (expr_of_typ quoter) args in
      let f = Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Suffix "of_rpc") lid)) in
      app f args
    | { ptyp_desc = Ptyp_variant (_, _, _); } ->
      failwith "Polymorphic variants not handled"
    | { ptyp_desc = Ptyp_any } ->
      failwith "Ptyp_any not handled"
    | { ptyp_desc = Ptyp_var name } ->
      [%expr [%e evar ("poly_"^name)]]
    | { ptyp_desc = Ptyp_poly (_, _) } ->
      failwith "Ptyp_poly not handled"
    | { ptyp_desc = Ptyp_extension _ } ->
      failwith "Ptyp_extension not handled"
    | { ptyp_desc = Ptyp_arrow (_, _, _) } ->
      failwith "Ptyp_arrow not handled"
    | { ptyp_desc = Ptyp_object (_, _) } ->
      failwith "Ptyp_object not handled"
    | { ptyp_desc = Ptyp_alias (_, _) } ->
      failwith "Ptyp_alias not handled"
    | { ptyp_desc = Ptyp_class (_, _) } ->
      failwith "Ptyp_class not handled"
    | { ptyp_desc = Ptyp_package _ } ->
      failwith "Ptyp_package not handled"

  let str_of_type ~options ~path type_decl =
    let quoter = Ppx_deriving.create_quoter () in
    let path = Ppx_deriving.path_of_type_decl ~path type_decl in
    let to_rpc =
      match type_decl.ptype_kind, type_decl.ptype_manifest with
      | Ptype_abstract, Some manifest ->
        expr_of_typ quoter manifest
      | Ptype_record labels, _ ->
        let fields =
          labels |> List.mapi (fun i { pld_name = { txt = name }; pld_type; pld_attributes } ->
              let rpc_name = attr_key name pld_attributes in
              let field =
                if is_option pld_type
                then
                  [%expr (if List.mem_assoc [%e str rpc_name] dict
                          then
                            [%e expr_of_typ quoter pld_type]
                              (Rpc.Enum [(List.assoc [%e str rpc_name] dict)])
                          else None)]
                else
                  [%expr [%e expr_of_typ quoter pld_type]
                           (List.assoc [%e str rpc_name] dict)]
              in name,field)
        in
        [%expr fun x -> match x with | Rpc.Dict dict -> [%e record fields] | _ -> failwith "expecting dict"]
      | Ptype_abstract, None ->
        failwith "Unhandled"
      | Ptype_open, _ ->
        failwith "Unhandled"
      | Ptype_variant constrs, _ ->
        let cases =
          constrs |> List.map (fun { pcd_name = { txt = name }; pcd_args; pcd_attributes } ->
              match pcd_args with
              | typs ->
                let args = List.mapi (fun i typ -> [%expr [%e expr_of_typ quoter typ] [%e evar (argn i)]]) typs in
                let subpattern = List.mapi (fun i _ -> pvar (argn i)) typs |> plist in
                let rpc_of = constr name args in
                let main = [%pat? Rpc.String [%p pstr name]] in
                let pattern = match args with
                  | [] -> main
                  | args -> [%pat? Rpc.Enum ([%p main] :: [%p subpattern])]
                in
                  Exp.case pattern rpc_of)
        in
        Exp.function_ cases              
    in to_rpc
end

  
module Rpc_of = struct
  let rec expr_of_typ quoter typ =
    match typ with
    | { ptyp_desc = Ptyp_constr ( { txt = lid }, args ) } when
        List.mem lid core_types ->
      [%expr Rpc.([%e Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix "rpc_of") lid))])]
    | { ptyp_desc = Ptyp_constr ( { txt = Lident "char" }, args ) } ->
      [%expr Rpc.(function c -> Rpc.Int (Int64.of_int (Char.code c)))]
    | [%type: [%t? typ] list] -> [%expr fun l -> Rpc.Enum (List.map [%e expr_of_typ quoter typ] l)]
    | [%type: [%t? typ] array] -> [%expr fun l -> Rpc.Enum (List.map [%e expr_of_typ quoter typ] (Array.to_list l))]
    | {ptyp_desc = Ptyp_tuple typs } ->
      let args = List.mapi (fun i typ -> app (expr_of_typ quoter typ) [evar (argn i)]) typs in
      [%expr fun [%p ptuple (List.mapi (fun i _ -> pvar (argn i)) typs)] ->
             Rpc.Enum [%e list args]]
    | [%type: [%t? typ] option] ->
      let e = expr_of_typ quoter typ in
      [%expr fun x -> match x with None -> Rpc.Enum [] | Some y -> Rpc.Enum [ [%e e] y ] ]
    | { ptyp_desc = Ptyp_constr ( { txt = lid }, args ) } ->
      let args = List.map (expr_of_typ quoter) args in
      let f = Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix "rpc_of") lid)) in
      app f args
    | { ptyp_desc = Ptyp_variant (_, _, _); } ->
      failwith "Polymorphic variants not handled"
    | { ptyp_desc = Ptyp_any } ->
      failwith "Ptyp_any not handled"
    | { ptyp_desc = Ptyp_var name } ->
      [%expr [%e evar ("poly_"^name)]]
    | { ptyp_desc = Ptyp_poly (_, _) } ->
      failwith "Ptyp_poly not handled"
    | { ptyp_desc = Ptyp_extension _ } ->
      failwith "Ptyp_extension not handled"
    | { ptyp_desc = Ptyp_arrow (_, _, _) } ->
      failwith "Ptyp_arrow not handled"
    | { ptyp_desc = Ptyp_object (_, _) } ->
      failwith "Ptyp_object not handled"
    | { ptyp_desc = Ptyp_alias (_, _) } ->
      failwith "Ptyp_alias not handled"
    | { ptyp_desc = Ptyp_class (_, _) } ->
      failwith "Ptyp_class not handled"
    | { ptyp_desc = Ptyp_package _ } ->
      failwith "Ptyp_package not handled"
  (*  | _ -> failwith "Error"*)

  let str_of_type ~options ~path type_decl =
    let quoter = Ppx_deriving.create_quoter () in
    let path = Ppx_deriving.path_of_type_decl ~path type_decl in
    let to_rpc =
      match type_decl.ptype_kind, type_decl.ptype_manifest with
      | Ptype_abstract, Some manifest ->
        expr_of_typ quoter manifest
      | Ptype_record labels, _ ->
        let fields =
          labels |> List.mapi (fun i { pld_name = { txt = name }; pld_type; pld_attributes } ->
              let rpc_name = attr_key name pld_attributes in
              if is_option pld_type
              then
                [%expr let rpc = [%e (expr_of_typ quoter pld_type)] [%e Exp.field (evar "x") (mknoloc (Lident name))] in
                       match rpc with | Rpc.Enum [x] -> Some ([%e str rpc_name], x) | Rpc.Enum [] -> None]
              else
                [%expr Some ([%e str rpc_name],
                             [%e (expr_of_typ quoter pld_type)] [%e Exp.field (evar "x") (mknoloc (Lident name))])]) in
        
        [%expr fun x -> Rpc.Dict (List.fold_right (fun x acc -> match x with | Some x -> x::acc | None -> acc) [%e list fields] []) ]
      | Ptype_abstract, None ->
        failwith "Unhandled"
      | Ptype_open, _ ->
        failwith "Unhandled"
      | Ptype_variant constrs, _ ->
        let cases =
          constrs |> List.map (fun { pcd_name = { txt = name }; pcd_args; pcd_attributes } ->
              match pcd_args with
              | typs ->
                let args = List.mapi (fun i typ -> [%expr [%e expr_of_typ quoter typ] [%e evar (argn i)]]) typs in
                let argsl = list args in
                let pattern = List.mapi (fun i _ -> pvar (argn i)) typs in
                let rpc_of = match args with
                  | [] -> [%expr Rpc.String [%e str name]]
                  | args -> [%expr Rpc.Enum ((Rpc.String [%e str name]) :: [%e argsl])]
                in
                Exp.case (pconstr name pattern) rpc_of)
        in
        Exp.function_ cases              
    in
    to_rpc

  
end

let strs_of_type ~options ~path type_decl =
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  [
    Vb.mk (pvar (Ppx_deriving.mangle_type_decl (`Prefix "rpc_of") type_decl)) (polymorphize (Rpc_of.str_of_type ~options ~path type_decl));
    Vb.mk (pvar (Ppx_deriving.mangle_type_decl (`Suffix "of_rpc") type_decl)) (polymorphize (Of_rpc.str_of_type ~options ~path type_decl));
  ]


let () =
  Ppx_deriving.(register (create deriver
                            ~core_type: (Ppx_deriving.with_quoter Rpc_of.expr_of_typ)
                            ~type_decl_str:(fun ~options ~path type_decls ->
                                
                                [Str.value Nonrecursive
                                   (List.concat (List.map (strs_of_type ~options ~path) type_decls))])
                            ()))
    
