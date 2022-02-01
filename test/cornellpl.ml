open Core
open Stdio
open Lib.Main

let rec fact n = if n = 1 then 1 else n * fact (n - 1)

let%test _ = fact 5 = 120

let%expect_test "value" =
  interp "22" |> print_endline;
  [%expect {| 22 |}]
;;
