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
  fs: Eio.Fs.dir_ty Eio.Path.t;
  pool: Eio.Executor_pool.t;
  limiter: Eio.Executor_pool.t;
  found: Lang.t list String.Table.t;
  ignore_cache: String.Set.t Lang.SetTable.t;
}

let rec traverse package directory =
  let dirs, files =
    Eio.Executor_pool.submit_exn package.limiter ~weight:0.01 (fun () ->
      Eio.Path.with_open_dir Eio.Path.(package.fs / directory) Eio.Path.read_dir
      |> List.partition_tf ~f:(fun filename ->
           Utils.Io.is_dir package.pool (Filename.concat directory filename) ) )
  in
  let detected = detect_lang files in
  (* Collect results *)
  Set.iter detected ~f:(fun lang -> Hashtbl.add_multi package.found ~key:directory ~data:lang);
  (* Recurse *)
  Set.diff (String.Set.of_list dirs) (get_ignore_dirs package.ignore_cache detected)
  |> Set.to_list
  |> Fiber.List.iter (fun dir -> traverse package (Filename.concat directory dir))

let find ~fs ~domain_mgr pool ~repo_root =
  Switch.run @@ fun sw ->
  let package =
    {
      fs;
      pool;
      limiter = Eio.Executor_pool.create ~sw ~domain_count:1 domain_mgr;
      found = String.Table.create ();
      ignore_cache = Lang.SetTable.create ();
    }
  in
  traverse package repo_root;
  package.found
