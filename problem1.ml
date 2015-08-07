open Printf

let main () =
  let sum = ref 0 in
  for i = 3 to 999 do
    if i mod 3 = 0 || i mod 5 = 0 then
    sum := !sum + i
  done;
  printf "answer: %d\n" !sum

let _ = main ()