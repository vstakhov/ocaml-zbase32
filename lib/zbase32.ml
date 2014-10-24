(*
 * Copyright (c) 2014 Vsevolod Stakhov <vsevolod@highsecure.ru>
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type t = string

let invalid_arg fmt =
  Printf.ksprintf (fun str -> raise (Invalid_argument str)) fmt

let b32a = "ybndrfg8ejkmcpqxot1uwisza345h769"

let of_string ?(pretty=false) s =
  let n = String.length s in
  let buf = Buffer.create (n*5/8) in
	let maxc = String.length b32a - 1 in
	let acc : int ref = ref (-1) in
  for i = 0 to n-1 do
		let c = Char.code s.[i] in 
		(match (i mod 5) with
		| 0 -> (Buffer.add_char buf b32a.[c land maxc] ; acc := c lsr 5)
		| 1 -> (let x = !acc lor (c lsl 3) in 
					Buffer.add_char buf b32a.[x land maxc] ;
					Buffer.add_char buf b32a.[(x lsr 5 land maxc)] ;
					acc := x lsr 10)
		| 2 -> (let x = !acc lor (c lsl 1) in 
					Buffer.add_char buf b32a.[x land maxc] ;
					acc := x lsr 5)
		| 3 -> (let x = !acc lor (c lsl 4) in 
					Buffer.add_char buf b32a.[x land maxc] ;
					Buffer.add_char buf b32a.[(x lsr 5 land maxc)] ;
					acc := x lsr 10 land 3)
		| 4 -> (let x = !acc lor (c lsl 2) in 
					Buffer.add_char buf b32a.[x land maxc] ;
					Buffer.add_char buf b32a.[(x lsr 5 land maxc)] ;
					acc := -1)
		| _ -> invalid_arg "Zbase32.of_string: invalid modulo" );
    if pretty then
      if (i+1) mod 27 = 0 then Buffer.add_char buf '\n'
      else if i+1 <> n then Buffer.add_char buf ' '
  done;
	if !acc >= 0 then
		Buffer.add_char buf b32a.[!acc land maxc] ;
  Buffer.contents buf

let can_skip = function
  | ' ' | '\t' | '\n' | '\r' | '-' -> true
  | _ -> false

(* ybndrfg8ejkmcpqxot1uwisza345h769 *)
let ab32 c = 
	match c with
	| 'y' -> 0
  | 'b' -> 1
	| 'n' -> 2
	| 'd' -> 3
	| 'r' -> 4
	| 'f' -> 5
	| 'g' -> 6
	| '8' -> 7
	| 'e' -> 8
	| 'j' -> 9
	| 'k' -> 10
	| 'm' -> 11
	| 'c' -> 12
	| 'p' -> 13
	| 'q' -> 14
	| 'x' -> 15
	| 'o' -> 16
	| 't' -> 17
	| '1' -> 18
	| 'u' -> 19
	| 'w' -> 20
	| 'i' -> 21
	| 's' -> 22
	| 'z' -> 23
	| 'a' -> 24
	| '3' -> 25
	| '4' -> 26
	| '5' -> 27
	| 'h' -> 28
	| '7' -> 29
	| '6' -> 30
	| '9' -> 31
	| _ -> -1

let rec to_string s =
  if s = "" then ""
  else
    let n = String.length s in
    let buf = Buffer.create (1 + n/8*5) in
		let rec process_input_c (i, (acc, pbits)) =
				if i < n then
					if not (can_skip s.[i]) then
  					match ab32 s.[i] with
  					| -1 -> process_input_c ((i + 1), (acc, pbits))						
  					| c -> process_input_c ((i + 1), (append_b32 buf c acc pbits))
					else
						process_input_c ((i + 1), (acc, pbits))
  			else
  				( if pbits > 0 then Buffer.add_char buf ((acc land 255) |> Char.chr) ;
  				n, (0, 0) )
		in
		process_input_c (0, (0, 0));
    Buffer.contents buf 
and append_b32 buf input acc processed_bits = 
	let nproc, nacc = 
  	if processed_bits >= 8 then
  		( Buffer.add_char buf (Char.chr (acc land 255)) ;
  		(processed_bits - 8), (acc lsr 8) )
  	else
  		processed_bits, acc
	in
	(nacc lor (input lsl nproc)), (nproc + 5)