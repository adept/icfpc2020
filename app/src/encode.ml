open Core

(*
Bits 0..1 define a positive or negative number (and signal width) via a high/low or low/high signal change:

01: positive number

10: negative number

Bits 2..(n+2) define the width of the following binary-encoded number via a unary-encoded number of length n composed of high signals ending with a low signal. The number width (in bits) is four times the unary encoding (i.e. 4 * n):

0: 0 [i.e. the number zero]

10: 4-bit number [i.e. 1-7]

110: 8-bit number [i.e. 1-255]

1110: 12-bit number [i.e. 1-4095]

…

The remaining bits, i.e. (n + 3)..(n + 3 + 4*n - 1), determine the number itself, in most-significant-bit first binary notation. Using the examples from this message:

0001: 1

00010000: 16

000100000000: 256

…

With this encoding, the number zero only requires three bits (i.e. 010), but arbitrarily large numbers can also be represented.
 *)

let nth_bit x n = x land (1 lsl n) <> 0

let encode n =
  let pos_neg = if n >= 0 then "01" else "10" in
  let rec bits x acc =
    if x = 0
    then (
      let extra_zeros =
        let rem = List.length acc mod 4 in
        if rem = 0 then 0 else 4 - rem
      in
      String.make extra_zeros '0' ^ String.of_char_list acc)
    else (
      let bit = if x mod 2 = 0 then '0' else '1' in
      bits (x / 2) (bit :: acc))
  in
  let bits = bits n [] in
  let four_bit_blocks = String.length bits / 4 in
  let len = if n = 0 then "0" else String.make four_bit_blocks '1' ^ "0" in
  pos_neg ^ len ^ bits
;;

let%expect_test "encode 0" =
  print_endline (encode 0);
  [%expect {|010|}];
  print_endline (encode 1);
  [%expect {|01100001|}];
  print_endline (encode 16);
  [%expect {|0111000010000|}];
  print_endline (encode 256);
  [%expect {|011110000100000000|}]
;;
