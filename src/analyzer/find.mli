open! Core

val find :
  fs:Eio.Fs.dir Eio.Path.t ->
  domain_mgr:#Eio.Domain_manager.t ->
  Dispatcher.t ->
  repo_root:string ->
  Lang.t list String.Table.t
