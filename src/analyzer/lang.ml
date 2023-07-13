open! Core

module Self = struct
  type t =
    | C_Sharp
    | NPM
  [@@deriving sexp, compare, hash]
end

include Self

module Set = struct
  include Set.Make (Self)

  let hash_fold_t state set = Set.fold set ~init:state ~f:hash_fold_t

  let hash = Hash.of_fold hash_fold_t
end

module SetTable = Hashtbl.Make (Set)

let base_ignore = String.Set.of_array [| ".git"; ".cache" |]

let ignore_dirs = function
| NPM -> String.Set.of_array [| "node_modules" |]
| C_Sharp ->
  String.Set.of_array
    [|
      "Debug";
      "debug";
      "DebugPublic";
      "debugPublic";
      "Release";
      "release";
      "Releases";
      "releases";
      "x64";
      "x86";
      "bld";
      "Bin";
      "bin";
      "test-bin";
      "Obj";
      "obj";
      "Log";
      "log";
    |]
