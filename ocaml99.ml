(* Lists: 1-28 *)

(* type ('a, 'b) result = | Result of 'a | Exn of 'b *)

(* 1: Tail of a List *)
let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t

let () = assert (last [ "a"; "b"; "c"; "d" ] = Some "d")
let () = assert (last [] = None)

(* 2: Last Two Elements of a List *)
let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t

let () = assert (last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d"))
let () = assert (last_two [ "a" ] = None)

(* 3: N'th Element of a List *)
let rec nth lst n =
  match (lst, n) with
  | h :: _, 0 -> h
  | [], _ -> raise (Failure "nth")
  | _ :: t, n -> nth t (n - 1)

let () = assert (nth [ "a"; "b"; "c"; "d"; "e" ] 2 = "c")
let () = assert ((try Some (nth [ "a" ] 2) with _ -> None) = None)

(* 4: Length of a List *)
let length lst =
  let rec aux lst acc =
    match lst with [] -> acc | _ :: t -> aux t (acc + 1)
  in
  aux lst 0

let () = assert (length [ "a"; "b"; "c" ] = 3)
let () = assert (length [] = 0)

(* 5: Reverse of a List *)
let rev lst =
  let rec aux lst acc =
    match lst with [] -> acc | h :: t -> aux t (h :: acc)
  in
  aux lst []

let () = assert (rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ])

(* 6: Palindrome *)
let is_palindrome lst = lst = rev lst
let () = assert (is_palindrome [ "x"; "a"; "m"; "a"; "x" ])
let () = assert (not (is_palindrome [ "a"; "b" ]))

(* 7: Flatten a List *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten lst =
  match lst with
  | [] -> []
  | h :: t -> (
      match h with
      | One h' -> h' :: flatten t
      | Many h' -> flatten h' @ flatten t)

let () =
  assert (
    flatten
      [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
    = [ "a"; "b"; "c"; "d"; "e" ])

(* 8: Eliminate Duplicates *)
let rec compress lst =
  match lst with
  | [] -> []
  | h1 :: h2 :: t when h1 = h2 -> compress (h1 :: t)
  | h1 :: t -> h1 :: compress t

let () =
  assert (
    compress
      [
        "a";
        "a";
        "a";
        "a";
        "b";
        "c";
        "c";
        "a";
        "a";
        "d";
        "e";
        "e";
        "e";
        "e";
      ]
    = [ "a"; "b"; "c"; "a"; "d"; "e" ])

(* 9: Pack Consecutive Duplicates *)
let rec pack (lst : string list) : string list list =
  match lst with
  | [] -> []
  | h1 :: h2 :: t when h1 = h2 -> (
      match pack (h2 :: t) with
      | [] -> raise (Failure "not supposed to happen")
      | h' :: t' -> (h1 :: h') :: t')
  | h1 :: t -> [ h1 ] :: pack t

let () =
  assert (
    pack
      [
        "a";
        "a";
        "a";
        "a";
        "b";
        "c";
        "c";
        "a";
        "a";
        "d";
        "d";
        "e";
        "e";
        "e";
        "e";
      ]
    = [
        [ "a"; "a"; "a"; "a" ];
        [ "b" ];
        [ "c"; "c" ];
        [ "a"; "a" ];
        [ "d"; "d" ];
        [ "e"; "e"; "e"; "e" ];
      ])

(* 10: Run-Length Encoding *)
let encode lst =
  let rec encode_helper = function
    | [] -> []
    | (c :: _ as h) :: t -> (length h, c) :: encode_helper t
    | [] :: _ -> raise (Failure "Not supposed to happen")
  in
  encode_helper (pack lst)

let () =
  assert (
    encode
      [
        "a";
        "a";
        "a";
        "a";
        "b";
        "c";
        "c";
        "a";
        "a";
        "d";
        "e";
        "e";
        "e";
        "e";
      ]
    = [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e") ])

(* 11: Modified Run-Length Encoding *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode_modified lst =
  let rec helper = function
    | [] -> []
    | (1, c) :: t -> One c :: helper t
    | (n, c) :: t -> Many (n, c) :: helper t
  in
  helper (encode lst)

let () =
  assert (
    encode_modified
      [
        "a";
        "a";
        "a";
        "a";
        "b";
        "c";
        "c";
        "a";
        "a";
        "d";
        "e";
        "e";
        "e";
        "e";
      ]
    = [
        Many (4, "a");
        One "b";
        Many (2, "c");
        Many (2, "a");
        One "d";
        Many (4, "e");
      ])

(* 12: Decode a Run-Length Encoded List *)
let decode (lst : 'a rle list) =
  let rec helper (n, c) =
    match n with
    | 0 -> []
    | n when n >= 0 -> c :: helper (n - 1, c)
    | _ -> raise (Failure "Not supposed to happen")
  in
  let helper2 = function One c -> (1, c) | Many (n, c) -> (n, c) in
  lst |> List.map helper2 |> List.map helper |> List.fold_left ( @ ) []

let () =
  assert (
    decode
      [
        Many (4, "a");
        One "b";
        Many (2, "c");
        Many (2, "a");
        One "d";
        Many (4, "e");
      ]
    = [
        "a";
        "a";
        "a";
        "a";
        "b";
        "c";
        "c";
        "a";
        "a";
        "d";
        "e";
        "e";
        "e";
        "e";
      ])

(* Arithmetic *)
(* 31: Determine Whether a Given Integer Number is Prime *)
let is_prime n =
  let rec helper curr n =
    if curr = 1 then true
    else if n mod curr = 0 then false
    else helper (curr - 1) n
  in
  if n <= 1 then false else helper (n - 1) n

let () = assert (not (is_prime 1))
let () = assert (is_prime 7)
let () = assert (not (is_prime 12))

(* 32: Determine the Greatest Common Divisor of Two Positive Integer
   Numbers *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let () = assert (gcd 13 27 = 1)
let () = assert (gcd 27 13 = 1)
let () = assert (gcd 20536 7826 = 2)
let () = assert (gcd 7826 20536 = 2)

(* 33: Determine Whether Two Positive Integer Numbers are Coprime *)
let coprime a b = gcd a b = 1
let () = assert (coprime 13 27)
let () = assert (not (coprime 20536 7826))

(* 34: Calculate Euler's Totient Function Φ(m) *)
let phi n =
  let rec aux curr acc =
    if curr <= 0 then acc
    else aux (curr - 1) (if coprime curr n then acc + 1 else acc)
  in
  aux (n - 1) 0

let () = assert (phi 10 = 4)

(* 35: Determine the Prime Factors of a Given Positive Integer *)
let rec factors n =
  let rec helper k n =
    if n = 1 then []
    else if n mod k = 0 then k :: factors (n / k)
    else helper (k + 1) n
  in
  if n <= 0 then failwith "blah" else helper 2 n

let () = assert (factors 315 = [ 3; 3; 5; 7 ])

(* 36: Determine the Prime Factors of a Given Positive Integer (2) *)
let rec factors2 n =
  let rec helper k n =
    if n = 1 then []
    else if n mod k = 0 then
      match factors2 (n / k) with
      | (k', n) :: t when k = k' -> (k', n + 1) :: t
      | lst -> (k, 1) :: lst
    else helper (k + 1) n
  in
  helper 2 n

let () = assert (factors2 315 = [ (3, 2); (5, 1); (7, 1) ])

(* 37: Calculate Euler's Totient Function Φ(m) (Improved) *)
let rec pow base exp =
  if exp < 0 then invalid_arg "Exponent must be non-negative"
  else if exp = 0 then 1
  else if exp mod 2 = 0 then
    let half = pow base (exp / 2) in
    half * half
  else base * pow base (exp - 1)

let () = assert (pow 2 3 = 8)
let () = assert (pow 6 2 = 36)
let () = assert (pow 2 10 = 1024)
let () = assert (pow 2 11 = 2048)

let phi_improved n =
  n |> factors2
  |> List.map (fun (p, m) -> (p - 1) * pow p (m - 1))
  |> List.fold_left ( * ) 1

let () = assert (phi_improved 10 = 4)
let () = assert (phi_improved 13 = 12)

(* 39: A List of Prime Numbers *)
let range a b =
  let rec aux i acc = if i < a then acc else aux (i - 1) (i :: acc) in
  aux b []

let all_primes lo hi = range lo hi |> List.filter is_prime
let () = assert (List.length (all_primes 2 7920) = 1000)

(* 40: Goldbach's Conjecture *)
let goldbach n =
  let rec help i n =
    if i > n then failwith "Conjecture false"
    else
      let j = n - i in
      if is_prime i && is_prime j then (i, j) else help (i + 1) n
  in
  if n <= 0 then invalid_arg "Positive only"
  else if n mod 2 = 1 then invalid_arg "Even only"
  else help 1 n

let () = assert (goldbach 28 = (5, 23))

(* 41: A List of Goldbach Compositions *)
let goldbach_list lo hi =
  range lo hi
  |> List.filter (fun n -> n mod 2 = 0)
  |> List.map (fun n -> (n, goldbach n))

let () =
  assert (
    goldbach_list 9 20
    = [
        (10, (3, 7));
        (12, (5, 7));
        (14, (3, 11));
        (16, (3, 13));
        (18, (5, 13));
        (20, (3, 17));
      ])

(* Logic and Codes: 4 *)
(* 46: Truth Tables for Logical Expressions (2 Variables) *)
type bool_exp =
  | Var of string
  | Not of bool_exp
  | And of bool_exp * bool_exp
  | Or of bool_exp * bool_exp

let rec eval e env =
  match e with
  | Var x -> List.assoc x env
  | Not e1 -> not (eval e1 env)
  | And (e1, e2) -> eval e1 env && eval e2 env
  | Or (e1, e2) -> eval e1 env || eval e2 env

let table2 x y e =
  [ (true, true); (true, false); (false, true); (false, false) ]
  |> List.map (fun (b1, b2) -> (b1, b2, eval e [ (x, b1); (y, b2) ]))

let () =
  assert (
    table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")))
    = [
        (true, true, true);
        (true, false, true);
        (false, true, false);
        (false, false, false);
      ])

let () =
  assert (
    table2 "a" "b" (Not (And (Var "a", Or (Var "a", Var "b"))))
    = [
        (true, true, false);
        (true, false, false);
        (false, true, true);
        (false, false, true);
      ])

(* Binary Trees: 17 *)
(* Multiway Trees: 5 *)
(* Graphs: 11 *)
(* 80: Conversions *)
type graph_t =
  | EdgeClause
  | GraphTerm
  | AdjacencyList
  | HumanFriendly

type 'a graph =
  | EdgeClause of (char * char) list
  | GraphTerm of {
      nodes : 'a list;
      edges : ('a * 'a) list;
    }
  | AdjacencyList of (char * char list) list
  | HumanFriendly of string

(* let rec convert_to (target_type : graph_t) g = match (g, target_type)
   with | HumanFriendly s, HumanFriendly -> g | HumanFriendly s,
   GraphTerm -> hf_to_gt s *)

(* and hf_to_gt s = let tokens = String.split_on_char ' ' s in List.map
   (fun t -> match String.length t with | 1 -> failwith "todo" | 3 ->
   failwith "todo" | _ -> failwith "not supposed to happen") *)

(* Miscellaneous: 9 *)
