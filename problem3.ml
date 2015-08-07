open Printf

let enum lo hi =
  Array.init (hi - lo + 1) (fun i -> lo + i)

let int_sqrt x =
  x |> float_of_int |> sqrt |> int_of_float

let for_all p arr = 
  let rec loop i =
    if i >= Array.length arr then true
    else p arr.(i) && loop (i + 1)
  in 
  loop 0

let is_prime p = 
  for_all (fun x -> p mod x <> 0) (enum 2 (int_sqrt p))

let main () =
  let target = ref 600851475143 in
  let i = ref 2 in
  while !i < !target do
    printf "%d\n" !i;
    if !target mod !i = 0 && is_prime !i then
      begin  
        while !target mod !i = 0 do
          target := !target / !i
        done
      end
    else i := !i + 1
  done;
  printf "answer %d\n" !i

let _ = main ()







