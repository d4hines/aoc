let year = 2023
let day = 6

module Part_1 = struct
  let run input =
    match String.split_on_char '\n' (String.trim input) with
    | [ times; distances ] ->
        let parse_line prefix line =
          print_endline line;
          let line = String.trim line in
          let prefix_len = String.length prefix in
          String.sub line prefix_len (String.length line - prefix_len)
          |> Str.split (Str.regexp "  *")
          |> List.map int_of_string
        in
        let times = parse_line "Time:" times in
        let distances = parse_line "Distance:" distances in
        let pairs = List.combine times distances in
        let product =
          List.fold_left
            (fun acc (time, distance) ->
              let x = ref 0 in
              for i = 1 to time - 1 do
                if i * (time - i) > distance then incr x else ()
              done;
              acc * !x)
            1 pairs
        in
        Ok (string_of_int product)
    | _ -> failwith "invalid input"

  let%expect_test "" =
    let input = {|
    Time:      7  15   30
    Distance:  9  40  200|} in
    Format.printf "%s\n" (run input |> Result.get_ok);
    [%expect
      {|
      Time:      7  15   30
          Distance:  9  40  200
      288 |}]
end

module Part_2 = struct
  (* y = (-b ± √(b^2 - 4ac)) / 2a *)
  let run input =
    match String.split_on_char '\n' (String.trim input) with
    | [ time; distance ] ->
        let parse_line prefix line =
          let line = String.trim line in
          let prefix_len = String.length prefix in
          let line =
            String.sub line prefix_len (String.length line - prefix_len)
          in
          let line = Str.global_replace (Str.regexp "  *") "" line in
          Float.of_string line
        in
        let time = parse_line "Time:" time in
        let distance = parse_line "Distance:" distance in
        let a = -1.0 in
        let b = time in
        let c = distance *. -1.0 in
        let discriminant = Float.sqrt (Float.pow b 2. -. (4. *. a *. c)) in
        let denom = a *. 2. in
        let start = ((-1. *. b) +. discriminant) /. denom in
        let end_ = ((-1. *. b) -. discriminant) /. denom in
        let range = Float.floor (end_ -. start) |> Float.to_int in
        Ok (string_of_int range)
    | _ -> failwith "invalid input"

  let%expect_test "" =
    let input = {|
    Time:      7  15   30
    Distance:  9  40  200|} in
    Format.printf "%s\n" (run input |> Result.get_ok);
    [%expect {| 71503 |}]
end
