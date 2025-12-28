open! Base

module Helpers = struct
  type 'a line_parser_rets =
    { caret : 'a
    ; dot : 'a
    ; s : 'a
    }

  let line_parser line_parser_rets line =
    List.map line ~f:(fun elem ->
      match elem with
      | '^' -> line_parser_rets.caret
      | '.' -> line_parser_rets.dot
      | 'S' -> line_parser_rets.s
      | _ ->
        failwith
          "Invalid input.txt format, character has to be element of {'^', '.', 'S'}")
  ;;

  let cons_splitter_mask lines =
    List.map lines ~f:(line_parser { caret = true; dot = false; s = false })
  ;;

  let cons_initial_beam_reg line =
    line_parser { caret = false; dot = false; s = true } line
  ;;

  let cons_initial_timelines line = line_parser { caret = 0; dot = 0; s = 1 } line

  (* List helpers for simulating bitwise logic *)
  let lwshl1 l = false :: List.drop_last_exn l
  let lwshr1 l = List.tl_exn l @ [ false ]
  let ( &&& ) = List.map2_exn ~f:( && )
  let ( ~~~ ) = List.map ~f:not
  let ( ||| ) = List.map2_exn ~f:( || )
  let lwpopcount l = List.count l ~f:Fn.id
  let cons_char_list input = List.map input ~f:String.strip |> List.map ~f:String.to_list

  (* Evaluate the next beam iteration. Returns a tuple of the next beam list and popcount *)
  let eval_next beam splitter =
    let hit_splitter = beam &&& splitter in
    let down_prop = beam &&& ~~~splitter in
    let next_iter = down_prop ||| lwshr1 hit_splitter ||| lwshl1 hit_splitter in
    next_iter, hit_splitter |> lwpopcount
  ;;

  (* Evaluate the next timeline iteration and return it *)
  let eval_next_timelines timelines splitter_mask =
    let bool_mask tl s = List.map2_exn tl s ~f:(fun x keep -> if keep then x else 0) in
    let hit_lst = bool_mask timelines splitter_mask in
    let pass_lst = bool_mask timelines ~~~splitter_mask in
    let hit_lst_left = 0 :: List.drop_last_exn hit_lst in
    let hit_lst_right = List.tl_exn hit_lst @ [ 0 ] in
    List.map3_exn pass_lst hit_lst_left hit_lst_right ~f:(fun p hl hr -> p + hl + hr)
  ;;

  let rec _part1aux beam popcount = function
    | splitter_mask :: tail ->
      let next_beam, local_popcount = eval_next beam splitter_mask in
      _part1aux next_beam (popcount + local_popcount) tail
    | [] -> popcount
  ;;

  let part1aux lines =
    _part1aux (cons_initial_beam_reg (List.hd_exn lines)) 0 (cons_splitter_mask lines)
  ;;

  let rec _part2aux timelines = function
    | splitter_mask :: tail ->
      let next_timelines = eval_next_timelines timelines splitter_mask in
      _part2aux next_timelines tail
    | [] -> List.fold timelines ~init:0 ~f:( + )
  ;;

  let part2aux lines =
    let splitter_masks = cons_splitter_mask lines in
    _part2aux (cons_initial_timelines (List.hd_exn lines)) splitter_masks
  ;;
end

let part1 input = Helpers.cons_char_list input |> Helpers.part1aux |> Int.to_string
let part2 input = Helpers.cons_char_list input |> Helpers.part2aux |> Int.to_string
