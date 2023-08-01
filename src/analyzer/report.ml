open! Core
open Problem
open Render.Markdown

let npm_base = Uri.of_string "https://www.npmjs.com"

let npm_uri ?(bold = true) name =
  Link
    {
      href = Uri.with_path npm_base (sprintf "/package/%s" name);
      label = (if bold then Bold name else Text name);
    }

let is_dev_only origins =
  Set.for_all origins ~f:(function
    | Origin.DevDependency _ -> true
    | _ -> false )

let render_origin ?bold = function
| Origin.Dependency s -> npm_uri ?bold s
| Origin.DevDependency s -> Many [ Bold "[dev]"; Text " "; npm_uri ?bold s ]

let render_version_ranges ~version_by_parent =
  Set.fold_right version_by_parent ~init:[] ~f:(fun { added_by_is_top; added_by; version; top } acc ->
    let item =
      match added_by_is_top with
      | true -> Many [ Code version; Text " by "; npm_uri ~bold:true added_by ]
      | false ->
        Many
          [
            Code version;
            Text " by ";
            npm_uri ~bold:false added_by;
            Text " (";
            render_origin top;
            Text ")";
          ]
    in
    item :: acc )
  |> Li

let render_other_versions ~main_version ~other_versions ~version_by_parent =
  let init =
    [ Details { summary = "View constraints"; contents = render_version_ranges ~version_by_parent } ]
  in
  Map.fold_right other_versions ~init ~f:(fun ~key:added_by ~data:{ version; is_top; _ } acc ->
    let item = Many [ Code version; Text " by "; npm_uri ~bold:is_top added_by ] in
    item :: acc )
  |> List.cons (Many [ Code main_version; Text " (shared)" ])
  |> Li

let security_problems_to_markdown problems =
  let rows =
    List.map problems
      ~f:(fun
           { info; dependency; origins; main_version; other_versions; version_by_parent; is_top_level; _ }
         ->
      let problem =
        match info with
        | { url = Some s; message; _ } -> Link { href = Uri.of_string s; label = Text message }
        | { message; _ } -> Text message
      in
      let fix =
        Option.value_map info.fix_available ~default:Nothing
          ~f:(fun { name; current_main_version; fixed_version; _ } ->
          Many
            [
              Text "Upgrade ";
              npm_uri name;
              Text " from ";
              Code current_main_version;
              Text " to ";
              Code fixed_version;
            ] )
      in
      let first_col =
        List.map info.affected ~f:(function
          | { name; is_top_level = true; _ } -> npm_uri name
          | { name; origins; is_top_level = false; _ } ->
            let origins =
              Set.fold_right origins ~init:[] ~f:(fun origin acc -> render_origin origin :: acc) |> Li
            in
            Many [ npm_uri ~bold:false name; origins ] )
        |> Li
      in
      let second_col =
        Many
          [
            npm_uri ~bold:is_top_level dependency;
            Text " ";
            Code info.range;
            render_other_versions ~main_version ~other_versions ~version_by_parent;
          ]
      in
      [
        first_col;
        second_col;
        (if is_dev_only origins then Bold "Yes" else Nothing);
        fix;
        Text info.severity;
        problem;
      ] )
  in
  match rows with
  | [] -> Block [ Text "No issues reported" ]
  | _ -> Table { headers = [ "Name"; "Cause"; "Dev only?"; "Fix"; "Severity"; "Problem" ]; rows }

let toplevel_problems_to_markdown problems =
  let rows =
    List.map problems ~f:(fun { info; dependency; origins; main_version; _ } ->
      [
        npm_uri dependency; Code main_version; (if is_dev_only origins then Bold "Yes" else Nothing); info;
      ] )
  in
  match rows with
  | [] -> Block [ Text "No issues reported" ]
  | _ -> Table { headers = [ "Name"; "Version"; "Dev only?"; "Problem" ]; rows }

let nested_problems_to_markdown problems =
  let by_origin =
    List.fold problems ~init:OriginMap.empty ~f:(fun init problem ->
      Set.fold problem.origins ~init ~f:(fun acc origin -> Map.add_multi acc ~key:origin ~data:problem) )
  in
  let rows =
    Map.fold_right by_origin ~init:[] ~f:(fun ~key:origin ~data:problems init ->
      List.fold_right problems ~init
        ~f:(fun { dependency; info; main_version; other_versions; version_by_parent; _ } acc ->
        [
          render_origin ~bold:true origin;
          npm_uri ~bold:false dependency;
          render_other_versions ~main_version ~other_versions ~version_by_parent;
          info;
        ]
        :: acc ) )
  in
  match rows with
  | [] -> Block [ Text "No issues reported" ]
  | _ -> Table { headers = [ "Top-level"; "Nested"; "Version(s)"; "Problem" ]; rows }

let to_markdown buf ~directory (problems : kind t list) =
  let security, toplevel, nested =
    List.partition3_map problems ~f:(function
      | { info = Security security; _ } as x -> `Fst { x with info = security }
      | { is_top_level; info = Outdated { latest; _ }; _ } as x ->
        let data = { x with info = Many [ Text "Latest: "; Code latest ] } in
        if is_top_level then `Snd data else `Trd data
      | { is_top_level; info = Deprecated s; _ } as x ->
        let data = { x with info = Text s } in
        if is_top_level then `Snd data else `Trd data )
  in

  render buf
    [
      Header (2, directory);
      Header (3, "Security issues");
      security_problems_to_markdown security;
      Header (3, "Top-level issues");
      toplevel_problems_to_markdown toplevel;
      Header (3, "Nested issues");
      nested_problems_to_markdown nested;
    ]

let all_to_markdown (problems : kind t list String.Map.t) =
  let buf = Buffer.create 8192 in

  (* Add header *)
  render buf
    [
      Header (1, "Results");
      Header (4, Time_float.now () |> Time_float.to_string_abs ~zone:Time_float.Zone.utc);
    ];

  Map.iteri problems ~f:(fun ~key:directory ~data -> to_markdown buf ~directory data);

  Buffer.contents buf
