type e =
	| Int of int
	| Str of string
	| Add of e * e
	| Mul of e * e

let rec show = function
	| Int i -> Printf.sprintf "%d" i
	| Str s -> Printf.sprintf "%S" s
	| Add(e1,e2) -> Printf.sprintf "{%s + %s}" (show e1) (show e2)
	| Mul(e1,e2) -> Printf.sprintf "{%s * %s}" (show e1) (show e2)
