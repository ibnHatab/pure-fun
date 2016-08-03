
open Core.Std

let build_counters () =
  In_channel.fold_lines stdin ~init:Counter.empty ~f:Counter.touch

let () =
  build_counters ()
  |> Counter.to_list
  |> List.sort ~cmp:(fun (_, x) (_, y) -> Int.descending x y)
  |> (fun counters -> List.take counters 10)
  |> List.iter ~f:(fun (line, count) -> printf "%3d: %s \n" count line)
