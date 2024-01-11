open! Core

(* TODO: This entire file can now be replaced with Eio calls *)

let stat_is_dir path =
  match Core_unix.stat path with
  | { st_kind = S_DIR; _ } -> Some true
  | _ -> Some false
  | exception _exn -> None

let is_dir pool path =
  Eio.Executor_pool.submit_exn pool ~weight:0.01 (fun () ->
    match Core_unix.stat path with
    | { st_kind = S_DIR; _ } -> true
    | _ -> false )

let create_dir_if_not_exists ~fs path ~perm =
  try Eio.Path.mkdir ~perm Eio.Path.(fs / path) with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ()

let file_exists pool path =
  Eio.Executor_pool.submit_exn pool ~weight:0.01 (fun () ->
    match Core_unix.stat path with
    | { st_kind = S_REG; _ } -> true
    | _ -> false
    | exception _ -> false )

let rec rm_rf ~fs target =
  match stat_is_dir target with
  | None -> ()
  | Some true ->
    Eio.Path.with_open_dir Eio.Path.(fs / target) Eio.Path.read_dir
    |> Eio.Fiber.List.iter ~max_fibers:2 (fun file -> rm_rf ~fs (Filename.concat target file));
    Eio.Path.rmdir Eio.Path.(fs / target)
  | Some false -> Eio.Path.unlink Eio.Path.(fs / target)
