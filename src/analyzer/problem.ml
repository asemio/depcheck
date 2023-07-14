open! Core

module Origin = struct
  type t =
    | Dependency of string
    | DevDependency of string
  [@@deriving sexp, compare]
end

module OriginSet = Set.Make (Origin)
module OriginMap = Map.Make (Origin)

type kind =
  | Deprecated of string
  | Outdated of {
      wanted: string;
      latest: string;
    }
[@@deriving sexp]

type t = {
  kind: kind;
  directory: string;
  lang: Lang.t;
  dependency: string;
  is_top_level: bool;
  version: string;
  origins: OriginSet.t;
}
[@@deriving sexp]

open Render.Markdown

let npm_base = Uri.of_string "https://www.npmjs.com"

let npm_uri ?(bold = true) name =
  Link
    {
      href = Uri.with_path npm_base (sprintf "/package/%s" name);
      label = (if bold then Bold name else Text name);
    }

let kind_to_markdown = function
| Deprecated s -> Text s
| Outdated { latest; _ } -> Many [ Text "Latest: "; Code latest ]

let toplevel_problems_to_markdown problems =
  let rows =
    List.map problems ~f:(fun { kind; dependency; origins; version; _ } ->
      let dev_only =
        Set.for_all origins ~f:(function
          | DevDependency _ -> true
          | _ -> false )
      in
      [
        npm_uri dependency;
        Code version;
        (if dev_only then Bold "Yes" else Nothing);
        kind_to_markdown kind;
      ] )
  in
  Table { headers = [ "Name"; "Version"; "Dev only?"; "Problem" ]; rows }

let nested_problems_to_markdown problems =
  let by_origin =
    List.fold problems ~init:OriginMap.empty ~f:(fun init problem ->
      Set.fold problem.origins ~init ~f:(fun acc origin -> Map.add_multi acc ~key:origin ~data:problem) )
  in
  let rows =
    Map.fold_right by_origin ~init:[] ~f:(fun ~key:origin ~data:problems init ->
      let source =
        match origin with
        | Dependency s -> npm_uri s
        | DevDependency s -> Many [ Bold "[dev]"; Text " "; npm_uri s ]
      in
      List.fold_right problems ~init ~f:(fun { dependency; version; kind; _ } acc ->
        [ source; npm_uri ~bold:false dependency; Code version; kind_to_markdown kind ] :: acc ) )
  in
  Table { headers = [ "Top-level"; "Nested"; "Version"; "Problem" ]; rows }

let to_markdown buf ~directory (problems : t list) =
  let toplevel, nested = List.partition_tf problems ~f:(fun { is_top_level; _ } -> is_top_level) in

  render buf
    [
      Header (2, directory);
      Header (3, "Top-level issues");
      toplevel_problems_to_markdown toplevel;
      Header (3, "Nested issues");
      nested_problems_to_markdown nested;
    ]

let all_to_markdown (problems : t list String.Map.t) =
  let buf = Buffer.create 8192 in

  (* Add header *)
  render buf
    [
      Header (1, "Results");
      Header (4, Time_float.now () |> Time_float.to_string_abs ~zone:Time_float.Zone.utc);
    ];

  Map.iteri problems ~f:(fun ~key:directory ~data -> to_markdown buf ~directory data);

  Buffer.contents buf
