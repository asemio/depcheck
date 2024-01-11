open! Core
open Eio.Std

let version = "0.1.0"

type settings = {
  repos: string list;
  debug: bool;
}

let minimal_npm_major_version = 7

let process_repo env ~debug pool arg_path ~just_one =
  let fs = Eio.Stdenv.fs env in
  let process_mgr = Eio.Stdenv.process_mgr env in
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let repo_root = Utils.External.run ~process_mgr [ "realpath"; arg_path ] |> String.rstrip in
  let found = Analyzer.Find.find ~fs ~domain_mgr pool ~repo_root in

  let problems =
    Switch.run @@ fun sw ->
    let npm_limiter = Eio.Executor_pool.create ~sw ~domain_count:1 domain_mgr in
    Hashtbl.to_alist found
    |> Fiber.List.filter_map ~max_fibers:2 (fun (key, (langs : Analyzer.Lang.t list)) ->
         let problems =
           List.map langs ~f:(function
             | NPM -> Analyzer.Npm.check ~debug ~fs ~process_mgr ~directory:key ~npm_limiter
             | C_Sharp -> [] )
           |> List.concat
         in
         let directory =
           match Filename.of_absolute_exn key ~relative_to:repo_root with
           | "." when Hashtbl.length found = 1 -> arg_path
           | relative -> relative
         in
         Option.some_if (not (List.is_empty problems)) (directory, problems) )
    |> String.Map.of_alist_exn
  in

  let target =
    match just_one with
    | true -> "depcheck-results.md"
    | false ->
      String.map arg_path ~f:(function
        | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.') as c -> c
        | _ -> '_' )
      |> sprintf "depcheck-results-%s.md"
  in
  traceln "Generating results: [%s]" target;
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644)
    Eio.Path.(fs / target)
    (fun file -> Eio.Flow.copy_string (Analyzer.Report.all_to_markdown problems) file)

let main settings env () =
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  (* Validate npm version *)
  let () =
    let out =
      Utils.External.run ~process_mgr:(Eio.Stdenv.process_mgr env) [ "npm"; "--version" ] |> String.rstrip
    in
    match String.split out ~on:'.' with
    | s :: _ ->
      if Int.of_string s < minimal_npm_major_version
      then failwithf "depcheck requires npm v7+ but this appears to be version '%s'" out ()
    | [] -> failwithf "Failed to get a valid npm version: '%s'" out ()
  in

  Switch.run @@ fun sw ->
  let pool = Eio.Executor_pool.create ~sw ~domain_count:4 domain_mgr in
  let just_one =
    match settings.repos with
    | [ _ ] -> true
    | _ -> false
  in
  Fiber.List.iter (process_repo env ~debug:settings.debug pool ~just_one) settings.repos;
  traceln "Done."

let () =
  let open Command in
  let open Command.Let_syntax in
  let common =
    let%map_open repos = "path" %: string |> sequence |> anon
    and debug =
      flag "--debug" ~aliases:[ "-d" ] ~full_flag_required:() no_arg
        ~doc:"Use this option when reporting bugs."
    in
    let repos = if List.is_empty repos then [ "." ] else repos in
    { repos; debug }
  in

  let handle_system_failure stderr = function
    | (Eio.Io _ as ex)
     |(Eio.Exn.Multiple _ as ex)
     |(Failure _ as ex)
     |(Core_unix.Unix_error _ as ex)
     |(Exn.Reraised _ as ex) ->
      Eio.Flow.copy_string (sprintf !"❌ An error occured:\n%{Utils.Exception}\n" ex) stderr;
      exit 1
    | exn -> raise exn
  in

  common
  >>| (fun common () ->
        Eio_main.run (fun env ->
          try main common env () with
          | exn when not common.debug -> handle_system_failure (Eio.Stdenv.stderr env) exn ))
  |> basic ~summary:"Check your dependencies - https://github.com/asemio/depcheck"
  |> Command_unix.run ~version
