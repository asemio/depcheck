open! Core

type inline =
  | Text of string
  | Bold of string
  | BoldI of inline
  | Italic of string
  | Code of string
  | Link of {
      href: Uri_sexp.t;
      label: inline;
    }
  | Nothing
  | Many of inline list
  | Li of inline list
  | Details of {
      summary: string;
      contents: inline;
    }
[@@deriving sexp]

type block =
  | Block of inline list
  | Header of int * string
  | Table of {
      headers: string list;
      rows: inline list list;
    }
[@@deriving sexp]

let render_inline buf inline =
  let rec loop = function
    | Text s -> Buffer.add_string buf (String.substr_replace_all s ~pattern:"|" ~with_:"\\|")
    | Bold s ->
      Buffer.add_string buf "**";
      loop (Text s);
      Buffer.add_string buf "**"
    | BoldI x ->
      Buffer.add_string buf "**";
      loop x;
      Buffer.add_string buf "**"
    | Italic s ->
      Buffer.add_char buf '*';
      loop (Text s);
      Buffer.add_char buf '*'
    | Code s ->
      Buffer.add_char buf '`';
      loop (Text s);
      Buffer.add_char buf '`'
    | Link { href; label } ->
      Buffer.add_char buf '[';
      loop label;
      bprintf buf !"](%{Uri})" href
    | Nothing -> ()
    | Many ll -> List.iter ll ~f:loop
    | Li ll ->
      Buffer.add_string buf "<ul>";
      List.iter ll ~f:(fun x ->
        Buffer.add_string buf "<li>";
        loop x;
        Buffer.add_string buf "</li>" );
      Buffer.add_string buf "</ul>"
    | Details { summary; contents } ->
      bprintf buf "<details><summary>%s</summary>" summary;
      loop contents;
      Buffer.add_string buf "</details>"
  in
  loop inline

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
