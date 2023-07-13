open! Core

let run ~process_mgr ?cwd args =
  let buf = Buffer.create 4096 in
  let stdout = Eio.Flow.buffer_sink buf in
  Eio.Process.run process_mgr ?cwd ~stdout args;
  Buffer.contents buf
