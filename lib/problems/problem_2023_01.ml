let year = 2023
let day = 1

module Part_1 = struct
  (* O(n) *)
  let rec tokenize acc (s : unit -> char option) =
    match s () with
    | None -> acc
    | Some c -> (
        match c with '0' .. '9' -> tokenize (c :: acc) s | _ -> tokenize acc s)

  let tokenize s = tokenize [] (String.to_seq s |> Seq.to_dispenser) |> List.rev

  let run (input : string) : (string, string) result =
    let sum = ref 0 in
    let lines = String.split_on_char '\n' (String.trim input) in
    List.iter
      (fun line ->
        let tokens = tokenize line in
        let first = List.hd tokens in
        let last = List.rev tokens |> List.hd in
        let i = int_of_string @@ Format.sprintf "%c%c" first last in
        sum := !sum + i)
      lines;
    Ok (string_of_int !sum)

  let%expect_test "" =
    let result =
      run {|1abc2
pqr3stu8vwx 
a1b2c3d4e5f 
treb7uchet 
    |} |> Result.get_ok
    in
    Format.printf "%s" result;
    [%expect {|
      142 |}]
end

module Part_2 = struct
  let match_token s =
    match s with
    | "0" -> Some '0'
    | "one" | "1" -> Some '1'
    | "two" | "2" -> Some '2'
    | "three" | "3" -> Some '3'
    | "four" | "4" -> Some '4'
    | "five" | "5" -> Some '5'
    | "six" | "6" -> Some '6'
    | "seven" | "7" -> Some '7'
    | "eight" | "8" -> Some '8'
    | "nine" | "9" -> Some '9'
    | _ -> None

  (* Surprisingly, also O(n).
     Although we backtrack, we do it at most 5 times (5 being the maximum
     token length, i.e [String.length "seven"]
  *)
  let tokenize s =
    let i = ref 0 in
    let j = ref 0 in
    let acc = ref [] in
    while !i < String.length s do
      match match_token (String.sub s !i !j) with
      | Some d ->
          incr i;
          j := 0;
          acc := d :: !acc
      | None ->
          if !i + !j = String.length s || !j > 5 then (
            incr i;
            j := 0)
          else incr j
    done;
    List.rev !acc

  let run (input : string) : (string, string) result =
    let sum = ref 0 in
    let lines = String.split_on_char '\n' (String.trim input) in
    List.iter
      (fun line ->
        let tokens = tokenize line in
        let first = List.hd tokens in
        let last = List.rev tokens |> List.hd in
        let i = int_of_string @@ Format.sprintf "%c%c" first last in
        sum := !sum + i)
      lines;
    Ok (string_of_int !sum)

  let%expect_test "tokenize" =
    let input = "eightwothree" in
    List.iter (Format.printf "%c") (tokenize input);
    [%expect {| 823 |}]

  let%expect_test "" =
    let input =
      {|two1nine
           eightwothree
           abcone2threexyz
           xtwone3four
           4nineeightseven2
           zoneight234
           7pqrstsixteen|}
    in
    let result = run input |> Result.get_ok in
    Format.printf "%s\n" result;
    assert (int_of_string result = 29 + 83 + 13 + 24 + 42 + 14 + 76);
    let input =
      {|9dlvndqbddghpxc
rtkrbtthree8sixfoureight6
fdxrqmfxdkstpmcj7lmphgsmqqnmjrtwo3tcbc
onetjcsmgk57nvmkvcvkdtqtsksgpchsfsjzkkmb
six8threepvlxttc85two
8five9ttqst2one2vz
hbrmhsnjeight64dgdnvdbspk7ninetzbvjczqrj|}
    in
    let result = run input |> Result.get_ok in
    Format.printf "%s\n" result;
    [%expect {|
      281
      458 |}]
end
