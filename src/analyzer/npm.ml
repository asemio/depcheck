open! Core
open Eio.Std

type 'a jobject = 'a String.Map.t [@@deriving sexp]

let jobject_of_yojson f = function
| `Assoc ll ->
  List.fold_result ll ~init:String.Map.empty ~f:(fun acc (key, json) ->
    Result.map (f json) ~f:(fun data -> Map.set acc ~key ~data) )
| json -> Error (sprintf !"Unexpected JSON for package.json.dependencies: %{Yojson.Safe}" json)

module Packagejson = struct
  type t = {
    name: string;
    dependencies: string jobject;
    dev_dependencies: string jobject; [@key "devDependencies"] [@default String.Map.empty]
  }
  [@@deriving sexp, of_yojson { strict = false }]

  let of_json_string raw = Yojson.Safe.from_string raw |> [%of_yojson: t] |> Result.ok_or_failwith

  let is_top_level packagejson name =
    Map.mem packagejson.dependencies name || Map.mem packagejson.dev_dependencies name
end

module Packagelockjson = struct
  module Raw = struct
    type package = {
      version: string;
      deprecated: string option; [@default None]
      dependencies: string jobject; [@default String.Map.empty]
      peer_dependencies: string jobject; [@key "peerDependencies"] [@default String.Map.empty]
      optional_dependencies: string jobject; [@key "optionalDependencies"] [@default String.Map.empty]
    }
    [@@deriving sexp, of_yojson { strict = false }]

    type t = {
      name: string;
      packages: package jobject; [@default String.Map.empty]
    }
    [@@deriving sexp, of_yojson { strict = false }]
  end

  type package = {
    version: string;
    deprecated: string option;
    dependencies: string String.Map.t;
    origins: Problem.OriginSet.t;
  }
  [@@deriving
    sexp,
      stable_record ~version:Raw.package ~remove:[ origins ]
        ~add:[ peer_dependencies; optional_dependencies ]
        ~modify:[ dependencies ]]

  type t = {
    name: string;
    packages: package String.Map.t;
    is_parent_of: string list String.Map.t;
  }
  [@@deriving sexp]

  (* Highest priority to the "right" *)
  let merge_dependencies ll =
    List.reduce ll ~f:(fun left right ->
      Map.merge left right ~f:(fun ~key:_ -> function
        | `Left x
         |`Right x
         |`Both (_, x) ->
          Some x ) )
    |> Option.value_exn ~here:[%here]

  let canonicalize =
    let re = Re.Perl.compile_pat "node_modules/" in
    (fun name -> Re.split re name |> List.last_exn)

  (* Highest priority to the "right" *)
  let merge_packages left right =
    {
      right with
      deprecated = Option.first_some right.deprecated left.deprecated;
      dependencies = merge_dependencies [ left.dependencies; right.dependencies ];
      origins = Set.union left.origins right.origins;
    }

  let of_json_string (packagejson : Packagejson.t) raw =
    let parsed = Yojson.Safe.from_string raw |> [%of_yojson: Raw.t] |> Result.ok_or_failwith in
    let packages_no_origins =
      Map.fold parsed.packages ~init:String.Map.empty ~f:(fun ~key ~data:raw acc ->
        match key with
        | "" -> acc
        | key -> (
          match String.chop_prefix key ~prefix:"node_modules/" with
          | None ->
            failwithf
              "package-lock.json dependency '%s' did not start with 'node_modules/'. Please report this \
               bug"
              key ()
          | Some key ->
            let realname = canonicalize key in
            let is_main = String.(realname = key) in
            let data =
              package_of_Raw_package raw ~origins:Problem.OriginSet.empty
                ~modify_dependencies:(fun dependencies ->
                merge_dependencies [ raw.peer_dependencies; raw.optional_dependencies; dependencies ] )
            in
            Map.update acc realname ~f:(function
              | None -> data
              | Some existing ->
                if is_main then merge_packages existing data else merge_packages data existing ) ) )
    in
    let is_parent_of =
      Map.fold packages_no_origins ~init:String.Map.empty
        ~f:(fun ~key:current ~data:{ dependencies; _ } acc ->
        Map.fold dependencies ~init:acc ~f:(fun ~key:dependency ~data:_ acc ->
          Map.add_multi acc ~key:dependency ~data:current ) )
    in
    let rec walk_down ~top acc name =
      match Map.find acc name with
      | None ->
        (* This can happen for some peerDependencies *)
        acc
      | Some found when Set.mem found.origins top -> acc
      | Some found ->
        let init =
          Map.change acc name ~f:(function
            | None -> assert false
            | Some package -> Some { package with origins = Set.add package.origins top } )
        in
        Map.fold found.dependencies ~init ~f:(fun ~key ~data:_ acc -> walk_down ~top acc key)
    in
    let packages =
      let init =
        Map.fold packagejson.dependencies ~init:packages_no_origins ~f:(fun ~key:top ~data:_ acc ->
          walk_down ~top:(Dependency top) acc top )
      in
      Map.fold packagejson.dev_dependencies ~init ~f:(fun ~key:top ~data:_ acc ->
        walk_down ~top:(DevDependency top) acc top )
    in
    { name = parsed.name; packages; is_parent_of }

  let get_package lock name =
    match Map.find lock.packages name with
    | None -> failwithf "Package not found: '%s'. Please report this bug." name ()
    | Some x -> x
end

module Outdated = struct
  type entry = {
    wanted: string;
    latest: string;
  }
  [@@deriving sexp, of_yojson { strict = false }]

  type t = entry jobject [@@deriving sexp, of_yojson]

  let of_json_string raw = Yojson.Safe.from_string raw |> [%of_yojson: t] |> Result.ok_or_failwith
end

let check ~fs ~process_mgr ~npm_limiter ~directory =
  let raw_packagejson = Eio.Path.load Eio.Path.(fs / directory / "package.json") in
  let packagejson = Packagejson.of_json_string raw_packagejson in

  (* traceln !"%{sexp#hum: Packagejson.t}" packagejson; *)

  (* Create temp directory and copy package.json into it *)
  let temp_dir =
    let dir = sprintf "/tmp/%d" ([%hash: string] directory) in
    Utils.Io.rm_rf ~fs dir;
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(fs / dir);
    Eio.Path.with_open_out ~create:(`Or_truncate 0o600)
      Eio.Path.(fs / dir / "package.json")
      (fun file -> Eio.Flow.copy_string raw_packagejson file);
    traceln "Analyzing [%s] at [%s]" directory dir;
    dir
  in

  (* Run [npm install] in temp directory *)
  let _output =
    Dispatcher.run_exn npm_limiter ~f:(fun () ->
      Utils.External.run ~process_mgr
        ~cwd:Eio.Path.(fs / temp_dir)
        [ "npm"; "install"; "--package-lock-only"; "--legacy-peer-deps"; "--json"; "--no-progress" ] )
  in

  (* traceln "NPM OUTPUT: %s" _output; *)

  (* Parse resulting package-lock.json *)
  let lock =
    Eio.Path.load Eio.Path.(fs / temp_dir / "package-lock.json")
    |> Packagelockjson.of_json_string packagejson
  in

  (* traceln !"%{sexp#hum: Packagelockjson.t}" lock; *)

  (* Run [npm outdated] in temp directory *)
  let outdated =
    Utils.External.run
      ~cwd:Eio.Path.(fs / temp_dir)
      ~process_mgr
      [ "npm"; "outdated"; "--json"; "-l"; "-a" ]
    |> Outdated.of_json_string
  in

  (* traceln !"%{sexp#hum: Outdated.t}" outdated; *)

  (* Generate a list of issues *)
  let problems =
    let init : Problem.t list = [] in
    (* Add outdated *)
    let init =
      Map.fold_right outdated ~init ~f:(fun ~key ~data:{ wanted; latest; _ } acc ->
        match String.( = ) wanted latest with
        | true -> acc
        | false ->
          let package = Packagelockjson.get_package lock key in
          {
            kind = Outdated { wanted; latest };
            directory;
            lang = NPM;
            dependency = key;
            is_top_level = Packagejson.is_top_level packagejson key;
            version = package.version;
            origins = package.origins;
          }
          :: acc )
    in
    (* Add deprecated *)
    let init =
      Map.fold_right lock.packages ~init ~f:(fun ~key ~data:{ deprecated; version; origins; _ } acc ->
        match deprecated with
        | None -> acc
        | Some msg ->
          {
            kind = Deprecated msg;
            directory;
            lang = NPM;
            dependency = key;
            is_top_level = Packagejson.is_top_level packagejson key;
            version;
            origins;
          }
          :: acc )
    in
    init
  in

  (* Clean up after ourselves *)
  Utils.Io.rm_rf ~fs temp_dir;
  traceln "Finished [%s]" directory;

  problems
