open! Base

module Helpers = struct
  let cons_splitter_mask lines =
    List.map
      lines
      ~f:
        (List.fold_right ~init:[] ~f:(fun elem tail ->
           let bit =
             match elem with
             | '^' -> true
             | '.' -> false
             | 'S' -> false
             | _ ->
               failwith
                 "Invalid input.txt format, character has to be element of {'^', '.', \
                  'S'}"
           in
           bit :: tail))
  ;;

  let cons_initial_beam_reg line =
    List.fold_right line ~init:[] ~f:(fun elem tail ->
      let bit =
        match elem with
        | '^' -> false
        | '.' -> false
        | 'S' -> true
        | _ ->
          failwith
            "Invalid input.txt format, character has to be element of {'^', '.', 'S'}"
      in
      bit :: tail)
  ;;

  (* List helpers for simulating bitwise logic *)
  let lwshl1 l = false :: List.drop_last_exn l
  let lwshr1 l = List.tl_exn l @ [ false ]
  let ( &&& ) = List.map2_exn ~f:( && )
  let ( ~~~ ) = List.map ~f:not
  let ( ||| ) = List.map2_exn ~f:( || )
  let lwpopcount l = List.count l ~f:Fn.id
  let cons_char_list input = List.map input ~f:String.strip |> List.map ~f:String.to_list

  (* Evaulate the next beam iteration. Returns a tuple of the next beam list and popcount *)
  let eval_next beam splitter =
    let hit_splitter = beam &&& splitter in
    let down_prop = beam &&& ~~~splitter in
    let next_iter = down_prop ||| lwshr1 hit_splitter ||| lwshl1 hit_splitter in
    next_iter, hit_splitter |> lwpopcount
  ;;

  let rec _part1aux beam popcount = function
    | splitter_mask :: tail ->
      let next_beams, local_popcount = eval_next beam splitter_mask in
      _part1aux next_beams (popcount + local_popcount) tail
    | [] -> popcount
  ;;

  let part1aux lines =
    _part1aux (cons_initial_beam_reg (List.hd_exn lines)) 0 (cons_splitter_mask lines)
  ;;
end

let part1 input = Helpers.cons_char_list input |> Helpers.part1aux |> Int.to_string

let part2 input =
  ignore input;
  ""
;;
