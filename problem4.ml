let arr_is_pallindrome arr =
  let len = Array.length arr in 
  let done = len / 2 - 1 in
  let loop i = 
    i = done || (arr.(i) = arr.(len - i) && loop (i + 1))
  in
  loop 0

let num_to_arr x = 
  let rec loop x acc =
    if x < 10 then x :: acc
    else loop (x / 10) (x mod 10 :: acc)
  in
  Array.of_list (loop x [])

let num_is_pallindrome = arr_is_pallindrome @@ num_to_arr

let main () =
  