type 'a param = {
  param_name : bytes;
  param_def : 'a Types.def;
}

module type RPC = sig
  type 'a res
  type 'a comp
  type _ fn
  val (@->) : 'a param -> 'b fn -> ('a -> 'b) fn
  val returning : 'a param -> 'a comp fn
  val declare : string -> string -> 'a fn -> 'a res
end

module GenClient : sig
  type 'a res = (Rpc.call -> Rpc.response Rpc.error_or) -> 'a
  type 'a comp = 'a Rpc.error_or
  type _ fn
  val (@->) : 'a param -> 'b fn -> ('a -> 'b) fn
  val returning : 'a param -> 'a comp fn
  val declare : string -> string -> 'a fn -> 'a res
end

module GenServer : sig
  type funcs = (string, Rpc.call -> Rpc.response Rpc.error_or) Hashtbl.t
  type 'a res = 'a -> funcs -> unit
  type 'a comp = 'a
  type _ fn
  val (@->) : 'a param -> 'b fn -> ('a -> 'b) fn
  val returning : 'a param -> 'a comp fn
  val declare : string -> string -> 'a fn -> 'a res
  val server : funcs -> Rpc.call -> Rpc.response Rpc.error_or
end


