open Ast 

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Ucparser.prog Uclexer.read lexbuf in
  ast
    
