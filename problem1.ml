open Printf

let main () =
  let sum = ref 0 in
  for i = 3 to 999 do
    sum := !sum + i
  done;
  printf "answer: %d\n" !sum

let _ = main ()