let year = 2023
let day = 2

type round = { red : int; green : int; blue : int }
[@@deriving show { with_path = false }]

type game = { id : int; rounds : round list }
[@@deriving show { with_path = false }]

type string_list = string list [@@deriving show { with_path = false }]

let parse_games s =
  List.fold_left
    (fun games line ->
      match String.split_on_char ':' line with
      | [ header; rounds ] ->
          let header_len = String.length header in
          let id = String.sub header 5 (header_len - 5) in
          let id = id |> String.trim |> int_of_string in
          let rounds =
            String.split_on_char ';' rounds
            |> List.map (fun round_str ->
                   let vals = String.split_on_char ',' round_str in
                   List.fold_left
                     (fun { red; green; blue } v ->
                       match String.trim v |> String.split_on_char ' ' with
                       | [ num; "green" ] ->
                           { red; green = int_of_string num; blue }
                       | [ num; "red" ] ->
                           { red = int_of_string num; green; blue }
                       | [ num; "blue" ] ->
                           { red; green; blue = int_of_string num }
                       | _ -> failwith "invalid round format")
                     { red = 0; green = 0; blue = 0 }
                     vals)
          in
          games @ [ { id; rounds } ]
      | _ -> failwith @@ Format.sprintf "invalid game format: %s" line)
    []
    (String.split_on_char '\n' (String.trim s))

module Part_1 = struct
  let max_red = 12
  let max_green = 13
  let max_blue = 14

  let run (input : string) : (string, string) result =
    let games = parse_games input in
    let sum =
      List.filter
        (fun { id = _; rounds } ->
          List.for_all
            (fun { red; green; blue } ->
              red <= max_red && green <= max_green && blue <= max_blue)
            rounds)
        games
      |> List.fold_left (fun acc { id; _ } -> id + acc) 0
    in
    Ok (string_of_int sum)

  let%expect_test "" =
    let input =
      {|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green|}
    in
    let result = run input |> Result.get_ok in
    Format.printf "%s\n" result;
    [%expect {|
      8 |}]
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let games = parse_games input in
    let power_sum =
      List.fold_left
        (fun acc game ->
          let { red; green; blue } =
            List.fold_left
              (fun { red; green; blue } r ->
                {
                  red = max red r.red;
                  green = max green r.green;
                  blue = max blue r.blue;
                })
              { red = 0; green = 0; blue = 0 }
              game.rounds
          in
          let power = red * green * blue in
          acc + power)
        0 games
    in
    Ok (string_of_int power_sum)
end
