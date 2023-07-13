open! Core
open Eio.Std

let get_ignore_dirs cache set =
  Hashtbl.find_or_add cache set ~default:(fun () ->
    Set.to_list set |> List.map ~f:Lang.ignore_dirs |> List.cons Lang.base_ignore |> String.Set.union_list )

let detect_lang filenames =
  List.fold filenames ~init:Lang.Set.empty ~f:(fun acc -> function
    | "package.json" -> Set.add acc NPM
    | filename when String.is_suffix filename ~suffix:".csproj" -> Set.add acc C_Sharp
    | _ -> acc )

type package = {
  fs: Eio.Fs.dir Eio.Path.t;
  dispatcher: Dispatcher.t;
  limiter: Dispatcher.t;
  found: Lang.t list String.Table.t;
  ignore_cache: String.Set.t Lang.SetTable.t;
}

let rec traverse package directory =
  let dirs, files =
    Dispatcher.run_exn package.limiter ~f:(fun () ->
      Eio.Path.with_open_dir Eio.Path.(package.fs / directory) Eio.Path.read_dir
      |> List.partition_tf ~f:(fun filename ->
           Utils.Io.is_dir package.dispatcher (Filename.concat directory filename) ) )
  in
  let detected = detect_lang files in
  (* Collect results *)
  Set.iter detected ~f:(fun lang -> Hashtbl.add_multi package.found ~key:directory ~data:lang);
  (* Recurse *)
  Set.diff (String.Set.of_list dirs) (get_ignore_dirs package.ignore_cache detected)
  |> Set.to_list
  |> Fiber.List.iter (fun dir -> traverse package (Filename.concat directory dir))

let find ~fs ~domain_mgr dispatcher ~repo_root =
  Switch.run @@ fun sw ->
  let package =
    {
      fs;
      dispatcher;
      limiter = Dispatcher.create ~sw ~num_domains:1 ~domain_concurrency:20 domain_mgr;
      found = String.Table.create ();
      ignore_cache = Lang.SetTable.create ();
    }
  in
  traverse package repo_root;
  package.found
