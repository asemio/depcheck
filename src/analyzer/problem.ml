open! Core

module Origin = struct
  type t =
    | Dependency of string
    | DevDependency of string
  [@@deriving sexp, compare]
end

module OriginSet = Set.Make (Origin)
module OriginMap = Map.Make (Origin)

type version_info = {
  added_by: string;
  added_by_is_top: bool;
  version: string;
  top: Origin.t;
}
[@@deriving sexp, compare]

module VersionInfoSet = Set.Make (struct
  type t = version_info [@@deriving sexp, compare]
end)

type fix = {
  name: string;
  current_main_version: string;
  fixed_version: string;
}
[@@deriving sexp]

type affected = {
  name: string;
  origins: OriginSet.t;
  is_top_level: bool;
}
[@@deriving sexp]

type security = {
  affected: affected list;
  severity: string;
  range: string;
  fix_available: fix option;
  message: string;
  url: string option;
}
[@@deriving sexp]

type outdated = {
  wanted: string;
  latest: string;
}
[@@deriving sexp]

type kind =
  | Deprecated of string
  | Outdated of outdated
  | Security of security
[@@deriving sexp]

type other_versions = {
  name: string;
  is_top: bool;
  version: string;
}
[@@deriving sexp]

type 'a t = {
  info: 'a;
  directory: string;
  lang: Lang.t;
  dependency: string;
  is_top_level: bool;
  main_version: string;
  other_versions: other_versions String.Map.t;
  version_by_parent: VersionInfoSet.t;
  origins: OriginSet.t;
}
[@@deriving sexp]
