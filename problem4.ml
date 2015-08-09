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

exception Finished

let rec inner i j = 
  if j < i 
  then None
  else 
    let ij = i * j in 
    if num_is_pallindrome ij
    then Some (ij, i, j)
    else inner i (j - 1)

let rec outer i = 
  if i < 100
  then None
  else 
    match inner i 999 with
    | None -> outer (i - 1)
    | answer -> answer

let main () =
  match outer 999 with
  | None -> printf "Nothing found\n"
  | Some (x, y, z) -> printf "answer: %d %d %d\n" x y z

let _ = main ()