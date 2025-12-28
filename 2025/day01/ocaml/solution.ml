open! Base

module Helpers = struct
  let concat_num chars =
    List.fold chars ~init:0 ~f:(fun acc c ->
      (acc * 10)
      + (Char.get_digit c
         |> Option.value_exn
              ~message:"Invalid input.txt format, expected digit after direction"))
  ;;

  let limiter n =
    let r = n % 100 in
    if r < 0 then r + 100 else r
  ;;

  (* Checks if rotation ended up on zero (part 1) *)
  let otz pos = if limiter pos = 0 then 1 else 0

  (* Returns number of clicks through zero (part 2) *)
  let gtz num = function
    | dir :: digits ->
      let dist = concat_num digits in
      if dist <= 0
      then 0
      else (
        let full = dist / 100 in
        let rem = dist % 100 in
        let pos = limiter num in
        let need =
          match dir with
          | 'R' ->
            let k = (100 - pos) % 100 in
            if k = 0 then 100 else k
          | 'L' ->
            let k = pos % 100 in
            if k = 0 then 100 else k
          | _ -> failwith "Invalid input.txt format, expected direction R or L"
        in
        full + if rem >= need then 1 else 0)
    | [] -> 0
  ;;

  let update_pos pos = function
    | dir :: digits ->
      (match dir with
       | 'L' -> pos - concat_num digits
       | 'R' -> pos + concat_num digits
       | _ -> failwith "Invalid input.txt format, expected direction R or L")
    | [] -> pos
  ;;

  let rec exec_rots pos zeroes func = function
    | rot :: t ->
      let npos = update_pos pos rot in
      exec_rots (limiter npos) (zeroes + func pos npos rot) func t
    | [] -> Int.to_string zeroes
  ;;

  let start_pos = 50
  let part1aux lst = exec_rots start_pos 0 (fun _pos npos _rot -> otz npos) lst
  let part2aux lst = exec_rots start_pos 0 (fun pos _npos rot -> gtz pos rot) lst
end

let parse_rots input = List.map input ~f:String.strip |> List.map ~f:String.to_list
let part1 input = parse_rots input |> Helpers.part1aux
let part2 input = parse_rots input |> Helpers.part2aux
