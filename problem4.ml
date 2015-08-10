open Printf

let arr_is_pallindrome arr =
  let len = Array.length arr in 
  let upper = len / 2 in
  let rec loop i = 
    i = upper || (arr.(i) = arr.(len - i - 1) && loop (i + 1))
  in
  loop 0

let num_to_arr x = 
  let rec loop x acc =
    if x < 10 then x :: acc
    else loop (x / 10) (x mod 10 :: acc)
  in
  Array.of_list (loop x [])

let num_is_pallindrome x = arr_is_pallindrome (num_to_arr x)

let rec inner best i j =
  if j < 900 then best
  else 
    let ij = i * j in
    let next = 
      if num_is_pallindrome ij && ij > best
      then ij
      else best
    in
    inner next i (j - 1)

let rec outer best i =
  if i < 900 
  then best
  else 
    let next = inner best i 999 in
    outer next (i - 1)

let main () =
  let answer = outer (-1) 999 in
  printf "answer: %d\n" answer

let _ = main ()