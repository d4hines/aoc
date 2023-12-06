let year = 2023
let day = 3

type char_option = char option [@@deriving show]

let is_symbol = function
  | '#' | '$' | '%' | '&' | '*' | '+' | '/' | '=' | '@' | '-' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let get_coord matrix i j =
  let rows = Array.length matrix in
  let columns = Bytes.length matrix.(0) in
  if i >= 0 && j >= 0 && i < rows && j < columns then
    Some (Bytes.get matrix.(i) j)
  else None

let check_adjacent_to_symbol matrix i j =
  let west = get_coord matrix i (j - 1) in
  let north = get_coord matrix (i - 1) j in
  let east = get_coord matrix i (j + 1) in
  let south = get_coord matrix (i + 1) j in
  let north_west = get_coord matrix (i - 1) (j - 1) in
  let north_east = get_coord matrix (i - 1) (j + 1) in
  let south_west = get_coord matrix (i + 1) (j - 1) in
  let south_east = get_coord matrix (i + 1) (j + 1) in
  List.exists
    (fun c -> match c with Some c when is_symbol c -> true | _ -> false)
    [ west; north; east; south; north_west; north_east; south_west; south_east ]

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let matrix =
      String.split_on_char '\n' (String.trim input)
      |> Array.of_list
      |> Array.map (fun line -> Bytes.of_string line)
    in
    let columns = Bytes.length matrix.(0) in
    let i = ref 0 in
    let j = ref 0 in
    let get_next_coord () = get_coord matrix !i !j in
    let digits = ref [] in
    let is_adjacent_to_symbol = ref false in
    let sum = ref 0 in
    let reset () =
      digits := [];
      is_adjacent_to_symbol := false
    in
    Seq.iter
      (fun c ->
        if is_digit c then (
          digits := c :: !digits;
          if check_adjacent_to_symbol matrix !i !j then
            is_adjacent_to_symbol := true);
        let at_east_edge = !j = columns - 1 in
        (* I think has a bug for column lenght 2! But whatever - my input is longer than that *)
        if
          (at_east_edge || (not @@ is_digit c))
          && !is_adjacent_to_symbol
          && List.length !digits > 0
        then (
          let len = List.length !digits in
          let num =
            let bytes = Bytes.create len in
            List.iteri (fun i c -> Bytes.set bytes i c) @@ List.rev !digits;
            Bytes.to_string bytes
          in
          let num = int_of_string num in
          (* Format.printf "Adding %d\n" num; *)
          sum := !sum + num;
          reset ());
        if (not @@ is_digit c) || at_east_edge then reset ();
        (* increment pointers, handling wrap around *)
        if at_east_edge then (
          incr i;
          j := 0)
        else incr j)
      (Seq.of_dispenser get_next_coord);
    Ok (string_of_int !sum)

  let%expect_test "" =
    let result =
      run
        {|
       467..114..
       ...*......
       ..35..633.
       ......#...
       617*......
       .....+.58.
       ..592.....
       ......755.
       ...$.*....
       .664.598..
       |}
      |> Result.get_ok
    in
    Format.printf "%s\n" result;
    let result_with_negative = run "*-1." |> Result.get_ok in
    Format.printf "%s\n" result_with_negative;
    let result_from_reddit =
      run
        {|
12.......*..
+.........34
.......-12..
..78........
..*....60...
78.........9
.5.....23..$
8...90*12...
............
2.2......12.
.*.........*
1.1..503+.56|}
      |> Result.get_ok
    in
    Format.printf "%s\n" result_from_reddit;
    let my_result =
      12 + 34 + 12 + 78 + 78 + 9 + 23 + 90 + 12 + 2 + 2 + 12 + 1 + 1 + 503 + 56
    in
    assert (my_result = int_of_string result_from_reddit);
    Format.printf "%d\n" my_result;
    [%expect {|
      0
      1
      925
      925 |}]
end

module Part_2 = struct
  let get_adjacent_digits matrix i j =
    let coords =
      [
        (i, j - 1);
        (i - 1, j);
        (i, j + 1);
        (i + 1, j);
        (i - 1, j - 1);
        (i - 1, j + 1);
        (i + 1, j - 1);
        (i + 1, j + 1);
      ]
    in
    List.filter_map
      (fun (i, j) -> Option.map (fun c -> (i, j, c)) @@ get_coord matrix i j)
      coords

  let run input =
    let matrix =
      String.split_on_char '\n' (String.trim input)
      |> Array.of_list
      |> Array.map (fun line -> Bytes.of_string line)
    in
    let columns = Bytes.length matrix.(0) in
    let i = ref 0 in
    let j = ref 0 in
    let get_next_coord () = get_coord matrix !i !j in
    let digits = ref [] in
    let reset () = digits := [] in
    let numbers = ref [] in
    let stars = ref [] in
    Seq.iter
      (fun c ->
        if is_digit c then digits := c :: !digits;
        if c = '*' then stars := (!i, !j) :: !stars;
        let at_east_edge = !j = columns - 1 in
        (* I think has a bug for column lenght 2! But whatever - my input is longer than that *)
        if (at_east_edge || (not @@ is_digit c)) && List.length !digits > 0 then (
          let len = List.length !digits in
          let num =
            let bytes = Bytes.create len in
            List.iteri (fun i c -> Bytes.set bytes i c) @@ List.rev !digits;
            Bytes.to_string bytes
          in
          numbers := (!i, !j - String.length num, num) :: !numbers;
          (* Format.printf "Adding %d\n" num; *)
          reset ());
        if (not @@ is_digit c) || at_east_edge then reset ();
        (* increment pointers, handling wrap around *)
        if at_east_edge then (
          incr i;
          j := 0)
        else incr j)
      (Seq.of_dispenser get_next_coord);
    (* List.iter *)
    (*   (fun (i, j, n) -> Format.printf "found number %s at (%d, %d)\n" n i j) *)
    (*   !numbers; *)
    let sum =
      List.fold_left
        (fun sum (star_i, star_j) ->
          (* Format.printf "considering star %d,%d\n" star_i star_j; *)
          let coords =
            [
              (star_i, star_j - 1);
              (star_i - 1, star_j);
              (star_i, star_j + 1);
              (star_i + 1, star_j);
              (star_i - 1, star_j - 1);
              (star_i - 1, star_j + 1);
              (star_i + 1, star_j - 1);
              (star_i + 1, star_j + 1);
            ]
          in
          let x =
            List.filter_map
              (fun (number_i, number_j, number_val) ->
                if
                  List.exists
                    (fun (direction_i, direction_j) ->
                      let row_close_enough =
                        number_j <= direction_j
                        && number_j + String.length number_val - 1
                           >= direction_j
                      in

                      if direction_i = number_i && row_close_enough then
                        (* let () = *)
                        (*   Format.printf *)
                        (* "direction: (%d,%d), number %s at (%d, %d), hdiff \ *)
                           (*      %d, row_close_enough: %b, column matches: %b\n" *)
                        (*     direction_i direction_j number_val number_i number_j *)
                        (*     (direction_j - number_j) row_close_enough *)
                        (*     (direction_i = number_i) *)
                        (* in *)
                        true
                      else false)
                    coords
                then Some number_val
                else None)
              !numbers
          in

          (* List.iter (fun n -> Format.printf "Found matching number: %s\n" n) x; *)
          match x with
          | [ a; b ] ->
              let a = int_of_string a in
              let b = int_of_string b in

              (* Format.printf "Found gear %d*%d = %d for star %d,%d\n" a b (a * b) *)
              (*   star_i star_j; *)
              sum + (a * b)
          | _ -> sum)
        0 !stars
    in
    Ok (string_of_int sum)

  let%expect_test "" =
    let input =
      {|
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..|}
    in
    Format.printf "%s\n" (run input |> Result.get_ok);
    [%expect {|
      467835 |}]
end
