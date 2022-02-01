open Core
open Stdio
open Lib.Main

let rec fact n = if n = 1 then 1 else n * fact (n - 1)

let%test _ = fact 5 = 120

let%expect_test "value" =
  interp "22" |> print_endline;
  [%expect {| 22 |}]
;;

let%expect_test "calculator" =
  interp "11+11" |> print_endline;
  [%expect {| 22 |}];

  interp "2*2*10" |> print_endline;
  [%expect {| 40 |}]

  (* interp "2+2*10" |> print_endline; *)

  (* interp "2*2+10" |> print_endline; *)

  (* interp "(10+1)+(5+6)" |> print_endline; *)
;;