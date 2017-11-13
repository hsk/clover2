type e =
	| Int of int
	| Reg of string
	| Add of e * e
	| Mul of e * e
	| Div of e * e
	| DivAssign of e * e
	
let rec show = function
	| Int i -> Printf.sprintf "%d" i
	| Reg s -> Printf.sprintf "Reg(%S)" s
	| Add(e1,e2) -> Printf.sprintf "(%s + %s)" (show e1) (show e2)
	| Mul(e1,e2) -> Printf.sprintf "(%s * %s)" (show e1) (show e2)
	| Div(e1,e2) -> Printf.sprintf "(%s / %s)" (show e1) (show e2)
	| DivAssign(e1,e2) -> Printf.sprintf "(%s /= %s)" (show e1) (show e2)
	