open! Core

val check :
  debug:bool ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  process_mgr:_ Eio.Process.mgr ->
  npm_limiter:Eio.Executor_pool.t ->
  directory:string ->
  Problem.kind Problem.t list
