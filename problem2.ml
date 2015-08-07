open Printf

let main () =
  let a, b = ref 1, ref 2 in
  let sum = ref 0 in
  while b < 4000001 do 
    if b mod 2 = 0 then sum := !sum + b;
    let tmp = !b in
    b := tmp + !a;
    a := tmp;
  done;
  printf "answer: %d\n" !sum

let _ = main ()