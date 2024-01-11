open! Core

let process_run process_mgr ?cwd ?stdin ?stdout ?stderr ?env ?executable ?is_success args =
  let open Eio.Process in
  Eio.Switch.run @@ fun sw ->
  let child = spawn ~sw process_mgr ?cwd ?stdin ?stdout ?stderr ?env ?executable args in
  match await child, is_success with
  | `Exited 0, None -> ()
  | `Exited code, Some f when f code -> ()
  | status, _ ->
    let ex = err (Child_error status) in
    raise (Exn.reraise ex (Format.asprintf "running command: %a" pp_args args))

let run ~process_mgr ?cwd ?(success_codes = [ 0 ]) args =
  let buf = Buffer.create 4096 in
  let stdout = Eio.Flow.buffer_sink buf in
  process_run process_mgr ?cwd ~stdout
    ~is_success:(fun code -> List.mem success_codes code ~equal:Int.equal)
    args;
  Buffer.contents buf
