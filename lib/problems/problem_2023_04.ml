let year = 2023
let day = 4

type char_option = char option [@@deriving show]

module Int_set = Set.Make (Int)

module Part_1 = struct
  let rec exp base exponent =
    match exponent with 0 -> 1 | _ -> base * exp base (exponent - 1)

  let run input =
    let sum =
      List.fold_left
        (fun sum line ->
          let line = String.split_on_char ':' line |> List.tl |> List.hd in
          match String.split_on_char '|' line with
          | [ left; right ] ->
              let to_int_set s =
                s |> String.split_on_char ' '
                |> List.filter_map (fun s ->
                       try Some (int_of_string s) with _ -> None)
                |> Int_set.of_list
              in
              let left = to_int_set left in
              let right = to_int_set right in
              let count = Int_set.inter left right |> Int_set.cardinal in
              let value = if count >= 1 then exp 2 (count - 1) else 0 in
              sum + value
          | _ -> failwith "invalid input")
        0
        (String.split_on_char '\n' (String.trim input))
    in
    Ok (string_of_int sum)

  (*   let%expect_test "" = *)
  (*     let input = *)
  (* {|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53 *)
     (* Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19 *)
     (* Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1 *)
     (* Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83 *)
     (* Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36 *)
     (* Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11|} *)
  (*     in *)
  (*     let result = run input |> Result.get_ok in *)
  (*     Format.printf "%s\n" result; *)
  (*     [%expect {| 13 |}] *)
end

type int_list = int list [@@deriving show]

module Part_2 = struct
  let parse input =
    List.fold_left
      (fun acc line ->
        let line = String.split_on_char ':' line |> List.tl |> List.hd in
        match String.split_on_char '|' line with
        | [ left; right ] ->
            let to_int_set s =
              s |> String.split_on_char ' '
              |> List.filter_map (fun s ->
                     try Some (int_of_string s) with _ -> None)
              |> Int_set.of_list
            in
            let left = to_int_set left in
            let right = to_int_set right in
            let count = Int_set.inter left right |> Int_set.cardinal in
            acc @ [ count ]
        | _ -> failwith "invalid input")
      []
      (String.split_on_char '\n' (String.trim input))

  let run input =
    let counts = parse input in
    (* Format.printf "counts: %a\n" pp_int_list counts; *)
    let total = ref (List.length counts) in
    let q = Queue.create () in
    List.iteri (fun i _ -> Queue.add i q) counts;
    while not @@ Queue.is_empty q do
      let n = Queue.pop q in
      let card_count = List.nth counts n in
      (* Format.printf "Popping %d with val %d\n%!" n card_count; *)
      (* Format.printf "Adding: "; *)
      for i = n + 1 to n + card_count do
        (* Format.printf "%d," i; *)
        incr total;
        Queue.add i q
      done
      (* Format.printf "\n%!" *)
    done;
    Ok (string_of_int !total)

  let%expect_test "" =
    let input =
      {|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11|}
    in
    let result = run input |> Result.get_ok in
    Format.printf "%s\n" result;
    [%expect {|
      30 |}]
end
