

(* Basic type definitions *)

type _ basic =
  | Int : int basic
  | Int32 : int32 basic
  | Int64 : int64 basic
  | Bool : bool basic
  | Float : float basic
  | String : string basic
  | Char : char basic
  
type _ typ =
  | Basic : 'a basic -> 'a typ
  | DateTime : string typ
  | Array : 'a typ -> 'a array typ
  | List : 'a typ -> 'a list typ
  | Dict : 'a basic * 'b typ -> ('a * 'b) list typ
  | Unit : unit typ
  | Option : 'a typ -> 'a option typ
  | Tuple : 'a typ * 'b typ -> ('a * 'b) typ
  | Struct : 'a structure -> 'a structure_value typ
  | Variant : 'a variant -> 'a variant_value typ

(* A type definition has a name and description *)
and 'a def = { name: string; description: string; ty: 'a typ; }
             
and 'a structure_value = { vfields : (string * Rpc.t) list }
and ('a, 's) field = {
  fname : string;
  fdescription : string;
  field : 'a typ;
}
and 'a boxed_field = BoxedField : ('a, 's) field -> 's boxed_field
and 'a structure = {
  sname : string;
  mutable fields: 'a boxed_field list
}
and ('a, 's) tag = {
  vname : string;
  vdescription : string;
  vcontents : 'a typ;
}
and 'a boxed_tag = BoxedTag : ('a, 's) tag -> 's boxed_tag
and 'a variant = {
  mutable variants : 'a boxed_tag list
}
and 'a variant_value = { tag : string; contents: Rpc.t }

(*let structure sname = Struct { sname; fields=[] }
let field : type t a. t structure_value typ -> string -> string -> a typ -> (a, t) field =
  fun structure fname fdescription field ->
    match structure with
    | Struct ({ sname; fields } as s) ->
      let field = { fname; fdescription; field } in
      s.fields <- (BoxedField field) :: s.fields;
      field
    | _ -> failwith "Unsupported"*)

let int    = { name="int";    ty=Basic Int;    description="Native integer" }
let int32  = { name="int32";  ty=Basic Int32;  description="32-bit integer"}
let int64  = { name="int64";  ty=Basic Int32;  description="64-bit integer"}
let bool   = { name="bool";   ty=Basic Bool;   description="Boolean"}
let float  = { name="float";  ty=Basic Float;  description="Floating-point number"}
let string = { name="string"; ty=Basic String; description="String"}
let char   = { name="char";   ty=Basic Char;   description="Char"}
let unit   = { name="unit";   ty=Unit;         description="Unit"}


let rec unmarshal : type a. a typ -> Rpc.t -> a Rpc.error_or = fun t v ->
  let open Rpc in
  let open Result in
  let lift f x = return (f x) in
  let list_helper typ l =
    List.fold_left (fun acc v ->
        match acc, unmarshal typ v with
        | (Ok a), (Ok v) -> Ok (v::a)
        | _ -> Error "Failed to unmarshal array")
      (Ok []) l
  in
  match t with
  | Basic Int -> int_of_rpc v
  | Basic Int32 -> int32_of_rpc v
  | Basic Int64 -> int64_of_rpc v
  | Basic Bool -> bool_of_rpc v
  | Basic Float -> float_of_rpc v
  | Basic String -> string_of_rpc v
  | Basic Char -> int_of_rpc v >>= lift Char.chr
  | DateTime -> dateTime_of_rpc v
  | Array typ -> begin
      match v with
      | Enum xs -> list_helper typ xs >>= lift Array.of_list
      | _ -> Error "Expecting Array"
    end
  | List typ -> begin
      match v with
      | Enum xs -> list_helper typ xs
      | _ -> Error "Expecting array"
    end
  | Dict (basic, typ) -> begin
      match v with
      | Dict xs -> begin
        match basic with
        | String ->
          let keys = List.map fst xs in
          let vs = List.map snd xs in
          list_helper typ vs >>= fun vs ->
          return (List.combine keys vs)
        | _ -> 
          Error "Expecting something other than a Dict type"
        end
      | _ ->
        Error "Unhandled"
    end
  | Unit -> unit_of_rpc v
  | Option _ ->
    Error "Unhandled"
  | Tuple (t1, t2) -> begin
      match v, t2 with
      | Rpc.Enum list, Tuple (_, _) ->
        unmarshal t1 (List.hd list) >>= fun v1 ->
        unmarshal t2 (Rpc.Enum (List.tl list)) >>= fun v2 ->
        Ok (v1, v2)
      | Rpc.Enum ([x;y]), _ ->
        unmarshal t1 x >>= fun v1 ->
        unmarshal t2 y >>= fun v2 ->
        Ok (v1,v2)
      | Rpc.Enum list, _ ->
        Error "Too many items in a tuple!"
      | _, _ ->
        Error "Expecting Rpc.Enum when unmarshalling a tuple"      
    end
  | Struct {sname; fields} -> begin
      match v with
      | Dict dict ->
        (* Check each field is OK *)
        List.fold_left (fun acc field ->
            acc >>= fun acclist ->
            let BoxedField f = field in
            let k = f.fname in
            if List.mem_assoc k dict
            then
              let v = List.assoc f.fname dict in
              (unmarshal f.field v >>= fun _ -> Ok ((k, v)::acclist))
            else
              Error (Printf.sprintf "Field '%s' not found when unmarshalling structure" f.fname))
          (Ok [])
          fields >>= fun vfields ->
        Ok { vfields }
      | _ -> 
        Error "Unhandled"
    end
  | Variant { variants } -> begin
      match v with
      | Rpc.String x ->
        if not (List.exists (fun (BoxedTag t) -> t.vname = x) variants)
        then Error (Printf.sprintf "Unknown variant: %s" x)
        else begin
          let BoxedTag tag = List.find (fun (BoxedTag t) -> t.vname = x) variants in
          match tag.vcontents with
          | Unit -> Ok ({ tag=tag.vname; contents=Rpc.Null }) 
          | _ -> Error (Printf.sprintf "Tag '%s' expects contents, none received" x)
        end
      | Rpc.Enum ((Rpc.String x)::xs) -> begin
          if not (List.exists (fun (BoxedTag t) -> t.vname = x) variants)
          then Error (Printf.sprintf "Unknown variant: %s" x)
          else
            let BoxedTag tag = List.find (fun (BoxedTag t) -> t.vname = x) variants in
            unmarshal tag.vcontents (Rpc.Enum xs) >>= fun _ ->
            Ok { tag=x; contents=Rpc.Enum xs }
        end
      | _ -> Error "Expecting a string or Enum"
    end
    
let rec marshal : type a. a typ -> a -> Rpc.t = fun t v ->
  let open Rpc in
  let open Result in
  match t with
  | Basic Int -> rpc_of_int v
  | Basic Int32 -> rpc_of_int32 v
  | Basic Int64 -> rpc_of_int64 v
  | Basic Bool -> rpc_of_bool v
  | Basic Float -> rpc_of_float v
  | Basic String -> rpc_of_string v
  | Basic Char -> rpc_of_int (Char.code v)
  | DateTime -> rpc_of_dateTime v
  | Array typ -> Enum (List.map (marshal typ) (Array.to_list v))
  | List typ -> Enum (List.map (marshal typ) v)
  | Dict (String, typ) ->
    Rpc.Dict (List.map (fun (k,v) -> (k, marshal typ v)) v)
  | Unit -> rpc_of_unit v
  | Option _ ->
    failwith "Unhandled"
  | Tuple (x, (Tuple (_,_) as y)) -> begin
      match marshal y (snd v) with
      | Rpc.Enum xs -> Rpc.Enum ((marshal x (fst v))::xs)
      | _ -> failwith "Marshalling a tuple should always give an Enum"
    end
  | Tuple (x, y) ->
    Rpc.Enum [marshal x (fst v); marshal y (snd v)]
  | Struct {sname; fields} -> begin
      let fields = List.map (function          
          | BoxedField f ->
            (f.fname, List.assoc f.fname v.vfields)) fields
      in Rpc.Dict fields
    end

let getf : type t a. (a, t) field -> t structure_value -> a Rpc.error_or =
  fun field v ->
    let v = List.assoc field.fname v.vfields in
    unmarshal field.field v

let setf : type t a. (a, t) field -> t structure_value -> a -> t structure_value =
  fun field str v ->
    { vfields = (field.fname, marshal field.field v) :: (List.filter (fun (name, _) -> name <> field.fname) str.vfields) }

let mkvar : type t a. (a, t) tag -> a -> t variant_value =
  fun variant contents -> 
  { tag = variant.vname; contents = marshal variant.vcontents contents }




type vm = {
  name_label : string;
  name_description : string;
}

(* fields *)
let vm_name_label : (string, vm) field = { fname="name_label"; fdescription=""; field=Basic String }
let vm_name_description : (string, vm) field = { fname="name_description"; fdescription=""; field=Basic String }

(* structure*)
let vm : vm structure = { sname="vm"; fields = [ BoxedField vm_name_label; BoxedField vm_name_description ] }

(* vm <-> vm structure *)
let vm_of_vm_structure vm_struct =
  let open Rpc in
  getf vm_name_label vm_struct >>= fun name_label ->
  getf vm_name_description vm_struct >>= fun name_description ->
  Result.Ok { name_label; name_description }
let vm_structure_of_vm vm =
  let vm_structure = { vfields = [] } in
  let vm_structure = setf vm_name_label vm_structure vm.name_label in
  let vm_structure = setf vm_name_description vm_structure vm.name_description in
  vm_structure

(* tydesc *)
let tydesc_of_vm = Struct vm
