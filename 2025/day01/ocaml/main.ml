let read file = In_channel.with_open_text file In_channel.input_lines

let report f input partnum =
  let start = Unix.gettimeofday () in
  let outp = f input in
  let stop = Unix.gettimeofday () in
  Printf.printf "Part%d executed in %.4fs:\n%s\n" partnum (stop -. start) outp;
  outp
;;

let () =
  let file = if Array.length Sys.argv > 1 then Sys.argv.(1) else "../input.txt" in
  let input = read file in
  if report Solution.part1 input 1 |> String.length > 0
  then report Solution.part2 input 2 |> ignore
;;
