open! Core
open Eio.Std

let version = "0.0.1"

type settings = {
  repos: string list;
  debug: bool;
}

let process_repo env dispatcher repo_root =
  let found = Analyzer.Find.find ~fs:env#fs ~domain_mgr:env#domain_mgr dispatcher ~repo_root in

  let problems =
    Switch.run @@ fun sw ->
    let npm_limiter = Dispatcher.create ~sw ~num_domains:1 ~domain_concurrency:1 env#domain_mgr in
    Hashtbl.to_alist found
    |> Fiber.List.filter_map ~max_fibers:2 (fun (key, (langs : Analyzer.Lang.t list)) ->
         let problems =
           List.map langs ~f:(function
             | NPM ->
               Analyzer.Npm.check ~fs:env#fs ~process_mgr:env#process_mgr ~directory:key ~npm_limiter
             | C_Sharp -> [] )
           |> List.concat
         in
         let directory = Filename.of_absolute_exn key ~relative_to:repo_root in
         Option.some_if (not (List.is_empty problems)) (directory, problems) )
    |> String.Map.of_alist_exn
  in

  let target = "depcheck-results.md" in
  traceln "Generating results: [%s]" target;
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644)
    Eio.Path.(env#fs / target)
    (fun file -> Eio.Flow.copy_string (Analyzer.Problem.all_to_markdown problems) file);

  traceln "Done."

let main settings env () =
  (* Validate npm version *)
  let () =
    let out = Utils.External.run ~process_mgr:env#process_mgr [ "npm"; "--version" ] in
    match String.split out ~on:'.' with
    | s :: _ ->
      if Int.of_string s < 7
      then failwithf "depcheck requires npm v7+ but this appears to be version '%s'" out ()
    | [] -> failwithf "Failed to get a valid npm version: '%s'" out ()
  in

  Switch.run @@ fun sw ->
  let dispatcher = Dispatcher.create ~sw ~num_domains:4 ~domain_concurrency:2 env#domain_mgr in
  Fiber.List.iter (process_repo env dispatcher) settings.repos

let () =
  let open Command in
  let open Command.Let_syntax in
  let common =
    let%map_open repos = "path" %: string |> sequence |> anon
    and debug =
      flag "--debug" ~aliases:[ "-d" ] ~full_flag_required:() no_arg
        ~doc:"Use this option when reporting bugs."
    in
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
        (* TODO: Revert to Eio_main.run once Eio 0.11 is released *)
        Eio_posix.run (fun env ->
          try main common env () with
          | exn when not common.debug -> handle_system_failure env#stderr exn ))
  |> basic ~summary:"Check your dependencies - https://github.com/asemio/depcheck"
  |> Command_unix.run ~version