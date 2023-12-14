open! Core
open Eio.Std
open Result.Let_syntax

type 'a jobject = 'a String.Map.t [@@deriving sexp]

let jobject_of_yojson f = function
| `Assoc ll ->
  List.fold_result ll ~init:String.Map.empty ~f:(fun acc (key, json) ->
    Result.map (f json) ~f:(fun data -> Map.set acc ~key ~data) )
| json -> Error (sprintf !"Unexpected JSON for package.json.dependencies: %{Yojson.Safe}" json)

module Packagejson = struct
  type t = {
    name: string option; [@default None]
    dependencies: string jobject; [@default String.Map.empty]
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

    type stub = {
      resolved: string;
      link: bool;
    }
    [@@deriving sexp, of_yojson]

    let package_or_stub_of_yojson = function
    | `Assoc [] -> Ok None
    | json -> (
      match package_of_yojson json with
      | Ok x -> Ok (Some x)
      | Error _ as x -> (
        match [%of_yojson: stub] json with
        | Ok _ -> Ok None
        | Error _ -> x ) )

    type t = {
      name: string;
      packages: (package option[@of_yojson package_or_stub_of_yojson]) jobject; [@default String.Map.empty]
    }
    [@@deriving sexp, of_yojson { strict = false }]
  end

  type package = {
    name: string;
    main_version: string;
    other_versions: Problem.other_versions String.Map.t;
    deprecated_main: string option;
    deprecated_specific: string option;
    dependencies: string String.Map.t;
    origins: Problem.OriginSet.t;
    version_by_parent: Problem.VersionInfoSet.t;
  }
  [@@deriving sexp]

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

  let re_canonicalize = Re.Perl.compile_pat "node_modules/"

  let canonicalize name =
    let chunks = Re.split re_canonicalize name in
    List.last_exn chunks, String.concat chunks

  (* Highest priority to the "right" *)
  let merge_packages left right =
    {
      right with
      deprecated_main = Option.first_some right.deprecated_main left.deprecated_main;
      deprecated_specific = Option.first_some right.deprecated_specific left.deprecated_specific;
      dependencies = merge_dependencies [ left.dependencies; right.dependencies ];
      origins = Set.union left.origins right.origins;
      other_versions =
        Map.merge left.other_versions right.other_versions ~f:(fun ~key -> function
          | `Left x
           |`Right x ->
            Some x
          | `Both (x, y) ->
            failwithf
              !"Duplicate other_versions (%s) '%{sexp: Problem.other_versions}' vs '%{sexp: \
                Problem.other_versions}'. Please report this bug."
              key x y () );
    }

  let of_json_string (packagejson : Packagejson.t) raw =
    let parsed_name, parsed_packages =
      match Yojson.Safe.from_string raw |> [%of_yojson: Raw.t] with
      | Ok { name; packages } ->
        let filtered =
          Map.filter_mapi packages ~f:(fun ~key ~data -> if String.is_empty key then None else data)
        in
        name, filtered
      | Error msg -> failwith msg
    in
    let packages_no_origins =
      Map.fold parsed_packages ~init:String.Map.empty ~f:(fun ~key ~data:raw acc ->
        let realname, specific_name = canonicalize key in
        let is_main = String.( = ) realname specific_name in
        let other_versions =
          if is_main
          then String.Map.empty
          else
            String.Map.singleton specific_name
              Problem.
                {
                  name = specific_name;
                  version = raw.version;
                  is_top = Packagejson.is_top_level packagejson specific_name;
                }
        in
        let data : package =
          {
            name = realname;
            main_version = raw.version;
            other_versions;
            deprecated_main = (if is_main then raw.deprecated else None);
            deprecated_specific = (if is_main then None else raw.deprecated);
            dependencies =
              merge_dependencies [ raw.peer_dependencies; raw.optional_dependencies; raw.dependencies ];
            origins = Problem.OriginSet.empty;
            version_by_parent = Problem.VersionInfoSet.empty;
          }
        in
        Map.update acc realname ~f:(function
          | None -> data
          | Some existing ->
            if is_main then merge_packages existing data else merge_packages data existing ) )
    in
    let is_parent_of =
      Map.fold packages_no_origins ~init:String.Map.empty
        ~f:(fun ~key:current ~data:{ dependencies; _ } acc ->
        Map.fold dependencies ~init:acc ~f:(fun ~key:dependency ~data:_ acc ->
          Map.add_multi acc ~key:dependency ~data:current ) )
    in
    let rec walk_down ~(added_by : Problem.version_info) acc name =
      match Map.find acc name with
      | None ->
        (* This can happen for some peerDependencies *)
        acc
      | Some found ->
        let init =
          Map.change acc name ~f:(function
            | None -> assert false
            | Some package ->
              Some
                {
                  package with
                  origins = Set.add package.origins added_by.top;
                  version_by_parent = Set.add package.version_by_parent added_by;
                } )
        in
        if Set.mem found.origins added_by.top
        then init
        else
          Map.fold found.dependencies ~init ~f:(fun ~key ~data:range acc ->
            let added_by =
              Problem.{ added_by_is_top = false; added_by = name; version = range; top = added_by.top }
            in
            walk_down ~added_by acc key )
    in
    let packages =
      let init =
        Map.fold packagejson.dependencies ~init:packages_no_origins ~f:(fun ~key:top ~data:range acc ->
          let added_by =
            Problem.{ added_by_is_top = true; added_by = top; version = range; top = Dependency top }
          in
          walk_down ~added_by acc top )
      in
      Map.fold packagejson.dev_dependencies ~init ~f:(fun ~key:top ~data:range acc ->
        let added_by =
          Problem.{ added_by_is_top = true; added_by = top; version = range; top = DevDependency top }
        in
        walk_down ~added_by acc top )
    in
    { name = parsed_name; packages; is_parent_of }

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

module Audit = struct
  module Severity = struct
    type t =
      | Other of string
      | Info
      | Low
      | Moderate
      | High
      | Critical
    [@@deriving sexp, compare]

    let to_string = function
    | Info -> "Info"
    | Low -> "Low"
    | Moderate -> "Moderate"
    | High -> "High"
    | Critical -> "Critical"
    | Other s -> s

    let of_yojson = function
    | `String "info" -> Ok Info
    | `String "low" -> Ok Low
    | `String "moderate" -> Ok Moderate
    | `String "high" -> Ok High
    | `String "critical" -> Ok Critical
    | `String s -> Ok (Other s)
    | json -> Error (sprintf !"Unexpected JSON for severity: '%{Yojson.Safe}'" json)
  end

  module Fix_available = struct
    type fix_available = {
      name: string;
      version: string;
    }
    [@@deriving sexp, of_yojson { strict = false }]

    type t = fix_available option [@@deriving sexp]

    let of_yojson = function
    | `Null
     |`Bool _ ->
      Ok None
    | `Assoc _ as json -> [%of_yojson: fix_available] json |> Result.map ~f:Option.return
    | json -> Error (sprintf !"Unexpected JSON for fix_available: '%{Yojson.Safe}'" json)
  end

  module Cvs = struct
    type cvs = {
      dependency: string;
      title: string option;
      url: string option;
      severity: Severity.t;
      range: string;
    }
    [@@deriving sexp, of_yojson { strict = false }]

    type t = cvs list [@@deriving sexp]

    let of_yojson = function
    | `List ll ->
      Ok
        (List.filter_map ll ~f:(function
          | `String _ -> None
          | json -> Some ([%of_yojson: cvs] json |> Result.ok_or_failwith) ) )
    | json -> Error (sprintf !"Unexpected JSON for via: '%{Yojson.Safe}'" json)
  end

  type entry = {
    name: string;
    severity: Severity.t;
    range: string;
    fix_available: Fix_available.t; [@default None] [@key "fixAvailable"]
    via: Cvs.t; [@default []]
    affected: string list; [@key "effects"]
  }
  [@@deriving sexp, of_yojson { strict = false }]

  type parsed = { vulnerabilities: entry jobject } [@@deriving sexp, of_yojson { strict = false }]

  type t = entry list [@@deriving sexp]

  let of_yojson yojson =
    let%map parsed = [%of_yojson: parsed] yojson in
    Map.data parsed.vulnerabilities
    |> List.map ~f:(fun entry ->
         {
           entry with
           affected = (if List.is_empty entry.affected then [ entry.name ] else entry.affected);
           via =
             List.sort entry.via ~compare:(fun { severity = x; _ } { severity = y; _ } ->
               [%compare: Severity.t] y x );
         } )
    |> List.sort ~compare:(fun { severity = x; _ } { severity = y; _ } -> [%compare: Severity.t] y x)

  let of_json_string raw = Yojson.Safe.from_string raw |> [%of_yojson: t] |> Result.ok_or_failwith
end

let check ~debug ~fs ~process_mgr ~npm_limiter ~directory =
  let raw_packagejson = Eio.Path.load Eio.Path.(fs / directory / "package.json") in
  let packagejson = Packagejson.of_json_string raw_packagejson in

  (* traceln !"%{sexp#hum: Packagejson.t}" packagejson; *)

  (* Create temp directory and copy package.json into it *)
  let temp_dir =
    let dir = sprintf "/tmp/%d" ([%hash: string] directory) in
    let perm = 0o700 in
    if debug
    then Utils.Io.create_dir_if_not_exists ~fs ~perm dir
    else (
      Utils.Io.rm_rf ~fs dir;
      Eio.Path.mkdir ~perm Eio.Path.(fs / dir) );
    Eio.Path.with_open_out ~create:(`Or_truncate 0o600)
      Eio.Path.(fs / dir / "package.json")
      (fun file -> Eio.Flow.copy_string raw_packagejson file);
    traceln "Analyzing [%s] at [%s]" directory dir;
    dir
  in

  (* Run [npm install] in temp directory *)
  let run_npm_install () =
    let _npm_install_output =
      Dispatcher.run_exn npm_limiter ~f:(fun () ->
        Utils.External.run ~process_mgr
          ~cwd:Eio.Path.(fs / temp_dir)
          [ "npm"; "install"; "--package-lock-only"; "--legacy-peer-deps"; "--json"; "--no-progress" ] )
    in
    ()
  in
  if not (debug && Utils.Io.file_exists npm_limiter (Filename.concat temp_dir "package-lock.json"))
  then run_npm_install ();

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
      ~process_mgr ~success_codes:[ 0; 1 ]
      [ "npm"; "outdated"; "--json"; "-l"; "-a" ]
    |> Outdated.of_json_string
  in

  (* traceln !"%{sexp#hum: Outdated.t}" outdated; *)

  (* Run [npm audit] in temp directory *)
  let audit =
    Dispatcher.run_exn npm_limiter ~f:(fun () ->
      Utils.External.run ~process_mgr
        ~cwd:Eio.Path.(fs / temp_dir)
        [ "npm"; "audit"; "--package-lock-only"; "--audit-level=none"; "--json"; "--no-progress" ] )
    |> Audit.of_json_string
  in

  (* traceln !"%{sexp#hum: Audit.t}" audit; *)

  (* Generate a list of issues *)
  let problems =
    let open Problem in
    let init = [] in
    (* Add outdated *)
    let init =
      Map.fold_right outdated ~init ~f:(fun ~key ~data:{ wanted; latest } acc ->
        let package = Packagelockjson.get_package lock key in
        let is_top_level = Packagejson.is_top_level packagejson key in
        let problem =
          {
            info = Outdated { wanted; latest };
            directory;
            lang = NPM;
            dependency = key;
            is_top_level;
            main_version = package.main_version;
            other_versions = package.other_versions;
            version_by_parent = package.version_by_parent;
            origins = package.origins;
          }
        in
        (* Add top_level problem *)
        let acc =
          if is_top_level && String.( <> ) package.main_version latest then problem :: acc else acc
        in
        (* Add nested problem *)
        let acc =
          if Map.exists package.other_versions ~f:(fun { is_top; version; _ } ->
               (not is_top) && String.( <> ) version latest )
          then { problem with is_top_level = false } :: acc
          else acc
        in
        acc )
    in
    (* Add deprecated *)
    let init =
      Map.fold_right lock.packages ~init ~f:(fun ~key ~data:package acc ->
        let acc =
          match package, Packagejson.is_top_level packagejson key with
          | { deprecated_main = Some msg1; deprecated_specific; _ }, true -> (
            let problem =
              {
                info = Deprecated msg1;
                directory;
                lang = NPM;
                dependency = key;
                is_top_level = true;
                main_version = package.main_version;
                other_versions = package.other_versions;
                version_by_parent = package.version_by_parent;
                origins = package.origins;
              }
            in
            match deprecated_specific with
            | None -> problem :: acc
            | Some msg2 -> problem :: { problem with info = Deprecated msg2; is_top_level = false } :: acc
            )
          | { deprecated_main = Some msg; _ }, false
           |{ deprecated_specific = Some msg; _ }, false
           |{ deprecated_main = None; deprecated_specific = Some msg; _ }, true ->
            {
              info = Deprecated msg;
              directory;
              lang = NPM;
              dependency = key;
              is_top_level = false;
              main_version = package.main_version;
              other_versions = package.other_versions;
              version_by_parent = package.version_by_parent;
              origins = package.origins;
            }
            :: acc
          | { deprecated_main = None; deprecated_specific = None; _ }, _ -> acc
        in
        acc )
    in
    (* Add audit *)
    let init =
      List.fold_right audit ~init
        ~f:(fun { name = _; severity = _; range = _; fix_available; via; affected } acc ->
        let fix_available =
          Option.map fix_available ~f:(fun { name; version } ->
            let package = Packagelockjson.get_package lock name in
            { name; fixed_version = version; current_main_version = package.main_version } )
        in
        let affected =
          List.map affected ~f:(fun key ->
            let package = Packagelockjson.get_package lock key in
            {
              name = package.name;
              origins = package.origins;
              is_top_level = Packagejson.is_top_level packagejson package.name;
            } )
        in
        List.fold_right via ~init:acc ~f:(fun { dependency; title; url; severity; range } acc ->
          let package = Packagelockjson.get_package lock dependency in
          {
            info =
              Security
                {
                  affected;
                  range;
                  severity = Audit.Severity.to_string severity;
                  fix_available;
                  message = Option.value title ~default:"Link";
                  url;
                };
            directory;
            lang = NPM;
            dependency;
            is_top_level = Packagejson.is_top_level packagejson dependency;
            main_version = package.main_version;
            other_versions = package.other_versions;
            version_by_parent = package.version_by_parent;
            origins = package.origins;
          }
          :: acc ) )
    in
    init
  in

  (* Clean up after ourselves *)
  if not debug then Utils.Io.rm_rf ~fs temp_dir;
  traceln "Finished [%s]" directory;

  problems
