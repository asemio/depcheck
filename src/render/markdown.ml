open! Core

type inline =
  | Text of string
  | Bold of string
  | Italic of string
  | Code of string
  | Link of {
      href: Uri_sexp.t;
      label: inline;
    }
  | Nothing
  | Many of inline list

type block =
  | Block of inline list
  | Header of int * string
  | Table of {
      headers: string list;
      rows: inline list list;
    }

let rec render_inline buf = function
| Text s -> Buffer.add_string buf s
| Bold s -> bprintf buf "**%s**" s
| Italic s -> bprintf buf "*%s*" s
| Code s -> bprintf buf "`%s`" s
| Link { href; label } ->
  Buffer.add_char buf '[';
  render_inline buf label;
  bprintf buf !"](%{Uri})" href
| Nothing -> ()
| Many ll -> List.iter ll ~f:(render_inline buf)

let render_block buf = function
| Block ll -> List.iter ll ~f:(render_inline buf)
| Header (i, s) -> bprintf buf "%s %s" (String.make i '#') s
| Table { headers; rows } ->
  let render_row row ~f =
    Buffer.add_string buf "|";
    List.iter row ~f:(fun x ->
      Buffer.add_char buf ' ';
      f buf x;
      Buffer.add_string buf " |" );
    Buffer.add_char buf '\n'
  in

  render_row headers ~f:Buffer.add_string;
  List.iter headers ~f:(fun _ -> Buffer.add_string buf "|-----");
  Buffer.add_string buf "|\n";
  List.iter rows ~f:(render_row ~f:render_inline)

let render buf doc =
  List.iter doc ~f:(fun block ->
    render_block buf block;
    Buffer.add_string buf "\n\n" )
