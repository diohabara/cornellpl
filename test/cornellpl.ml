open Core
open Stdio
open Lib.Main

let print_interp (s : string) = interp s |> print_endline

let%expect_test "value" =
  interp "22" |> print_endline;
  [%expect {| 22 |}]
;;

let%expect_test "calculator" =
  interp "11+11" |> print_endline;
  [%expect {| 22 |}];
  interp "2*2*10" |> print_endline;
  [%expect {| 40 |}];
  interp "2+2*10" |> print_endline;
  [%expect {| 22 |}];
  interp "2*2+10" |> print_endline;
  [%expect {| 14 |}];
  interp "(10+1)+(5+6)" |> print_endline;
  [%expect {| 22 |}]
;;

let%expect_test "simple programming language" =
  print_interp "if true then 22 else 0";
  [%expect {| 22 |}];
  print_interp "true";
  [%expect {| true |}];
  print_interp "1 <= 1";
  [%expect {| true |}];
  print_interp "if 1+2 <= 3+4 then 22 else 0";
  [%expect {| 22 |}];
  print_interp "if 1+2 <= 3*4 then let x = 22 in x else 0";
  [%expect {| 22 |}];
  print_interp "let x = 1+2 <= 3*4 in if x then 22 else 0";
  [%expect {| 22 |}]
;;
