open! Core

val check :
  debug:bool ->
  fs:#Eio.Fs.dir Eio.Path.t ->
  process_mgr:#Eio.Process.mgr ->
  npm_limiter:Dispatcher.t ->
  directory:string ->
  Problem.kind Problem.t list
