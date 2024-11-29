(***************** Using the Lazy module ******************)
(* Here we provide an alternate implementation of streams using
   OCaml's lazy module. We recommend that you explore the
   documentation here:
     http://caml.inria.fr/pub/docs/manual-ocaml/libref/Lazy.html

   In this portion, you will be reimplementing various functions that
   were defined in class and several more ...
*)

(***************** Using the Num module ******************)
(* This file uses OCaml's Zarith library for arbitrary-precision arithmetic.
 * All functions that you write where you might be inclined to use the 
 * simple int type as the type of stream should instead use the Z.t type
 * The type's API is here:
 *   https://antoinemine.github.io/Zarith/doc/latest/Z.html
 *   https://github.com/ocaml/Zarith/blob/master/z.mli
 *
 * This should already be installed and accessible, and the lib/dune
 * file mentions Zarith.  But you may need to install the library with
 *   % opam install Zarith
 * (If you find that's the case, post a note on Ed)
 *  
 * Some useful operators from the Z library include:
 *  Z.add or (+) addition on Z.t
 *  Z.sub or (-) subtraction on Z.t
 *  Z.mul or ( * ) multiplication on Z.t  [didn't want to end the comment!]
 *  Z.div or (/) division on Z.t
 *  Z.pow     power on Z.t
 *
 * See the documentation for more.
 * 
 *  You could do "open Z" if you want, but then all of Z's infix operators
 *  will hide the ordinary +,-,*, etc. on the int type.  So you have three
 * choices:
 *  1. open Z  anyway
 *  2. Don't open Z, and use  Z.add, Z.sub, Z.leq etc. as prefix operators
 *  3. Don't open Z, and use this feature:  Z.(a*x+b)  interprets infix * and +
 *       as defined in the Z module
 *)

open Lazy

(* some useful numbers *)
let zero  : Z.t = Z.zero
let one   : Z.t = Z.one
let two   : Z.t = Z.of_int 2
let three : Z.t = Z.of_int 3
let four  : Z.t = Z.of_int 4
let five  : Z.t = Z.of_int 5

type 'a str = Cons of 'a * 'a stream
and 'a stream = 'a str Lazy.t

(* a stream of ones *)
let rec ones : Z.t stream = 
  lazy (
    Cons (one, ones)
  )

let unimplemented = Failure "unimplemented"

(*>* Problem 2.1.a *>*)
(* Implement the head and tail functions *)

let head (s:'a stream) : 'a =
  raise unimplemented

let tail (s:'a stream) : 'a stream =
  raise unimplemented

(*>* Problem 2.1.b *>*)
(* Implement map *)

let rec map (f:'a -> 'b) (s:'a stream) : 'b stream =
  raise unimplemented

(*>* Problem 2.1.c *>*)
(* Write a function nth, which returns the nth element of a
   stream. NOTE: the function nth should be zero-indexed. In other
   words, "nth 0 s" should be equivalent to "head s". *)

let rec nth (n:Z.t) (s:'a stream) : 'a =
  raise unimplemented




(*>* Problem 2.2 *>*)

(* Define a type for an infinite spreadsheet full of cells with type 'a. 
 *
 * You are free to use any representation of infinite spreadsheets you choose.
 *
 * Each cell in the spreadsheet will have coordinates (i,j).  You can think
 * of a cell with coordinates (i,j) as the cell inhabiting the ith row and
 * jth column of the spreadsheet.  You can assume that (i,j) are both
 * non-negative.  Indices in the spreadsheet start at 0.

 * Coordinates will be represented using OCaml's arbitrary precision Num
 * library, as in the rest of this file.

 * Such a spreadsheet will need to support the following operations:
 *)

type 'a spread_sheet = Z.t (* change me! *)

(* you can assume all coordinates given are non-negative *)
type coordinates = Z.t * Z.t 

(* a spreadsheet containing all zeros *)
let zeros : Z.t spread_sheet = zero (* change me! *)

(* return the element at the (i,j) coordinate in ss *)
let get ((i,j):coordinates) (ss:'a spread_sheet) : 'a = 
  raise unimplemented

(* create a new spreadsheet where the (i,j) element of the spreadsheet
 * contains f i j xij  when xij was the (i,j) element of the input spreadsheet
 *)
let map_all (f:Z.t -> Z.t -> 'a -> 'b) (ss:'a spread_sheet) : 'b spread_sheet = 
  raise unimplemented

(* create an infinite multiplication table in which every cell contains the
 * product of its indices *)
let multiplication_table = zero (* change me *)



