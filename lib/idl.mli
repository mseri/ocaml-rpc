module Param : sig
  type 'a t = {
    name : bytes;
    description : bytes;
    typedef : 'a Types.def;
  }
  type boxed = Boxed : 'a t -> boxed

  val mk : ?name:string -> ?description:string -> 'a Types.def -> 'a t
end

module Interface : sig
  type description = {
    name : string;
    description : string;
    version : int;
  }
end

module type RPC = sig
  type description
  val describe : Interface.description -> description

  type 'a res
  type 'a comp
  type _ fn
  val (@->) : 'a Param.t -> 'b fn -> ('a -> 'b) fn
  val returning : 'a Param.t -> 'a comp fn
  val declare : string -> string -> 'a fn -> 'a res
end

module GenClient : sig
  type description = Interface.description
  val describe : Interface.description -> description

  type 'a res = (Rpc.call -> Rpc.response Rpc.error_or) -> 'a
  type 'a comp = 'a Rpc.error_or
  type _ fn
  val (@->) : 'a Param.t -> 'b fn -> ('a -> 'b) fn
  val returning : 'a Param.t -> 'a comp fn
  val declare : string -> string -> 'a fn -> 'a res
end

module GenServer : sig
  type description = Interface.description
  val describe : Interface.description -> description

  type funcs = (string, Rpc.call -> Rpc.response Rpc.error_or) Hashtbl.t
  type 'a res = 'a -> funcs -> unit
  type 'a comp = 'a
  type _ fn
  val (@->) : 'a Param.t -> 'b fn -> ('a -> 'b) fn
  val returning : 'a Param.t -> 'a comp fn
  val declare : string -> string -> 'a fn -> 'a res
  val server : funcs -> Rpc.call -> Rpc.response Rpc.error_or
end


