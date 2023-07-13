open! Core

type t =
  | C_Sharp
  | NPM
[@@deriving sexp, compare, hash]

module Set : sig
  include Set.S with type Elt.t := t

  val hash_fold_t : Hash.state -> t -> Hash.state

  val hash : t -> Hash.hash_value
end

module SetTable : Hashtbl.S with type key := Set.t

val base_ignore : String.Set.t

val ignore_dirs : t -> String.Set.t
