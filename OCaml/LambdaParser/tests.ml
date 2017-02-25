open LambdaParser;;

let fst t =
	let (x, y) = t in
	x;;

let snd t =
	let (x, y) = t in
	y;;

(* Tests for parser *)
(* print_string (string_of_lambda (lambda_of_string "\\x.\\y.xy")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "xy")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "(x)")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "(((((((\\y.y)))))))")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "((z))(\\x.\\y.((xy)))")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "\\l.\\i.\\f.\\e.(l)(i)(f)(esgood)")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "\\x.\\y.xy")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "x1")); print_string "\n";; *)

(* Tests for is_alpha_equivalent function *)

let tester_eq tup =
	print_string 
		(string_of_bool
			(is_alpha_equivalent
				(fst tup)
				(snd tup))); print_string "\n";;

let t1 = (lambda_of_string "(x)", lambda_of_string "(y)");;
let t2 = (lambda_of_string "xy", lambda_of_string "xy");;
let t3 = (lambda_of_string "\\x.xy", lambda_of_string "\\y.yy");;
let t4 = (lambda_of_string "\\x.x", lambda_of_string "\\y.y");;
let t5 = (lambda_of_string "(\\x.x)(z)(w)", lambda_of_string "(\\y.y)(z)(w)");;
let t6 = (lambda_of_string "\\x1.\\x2.\\x3.\\x4.x1x2x3x4", lambda_of_string "\\y1.\\y2.\\y3.\\y4.y1y2y3y4");;
let t7 = (lambda_of_string "\\x1.\\x2.\\x3.\\x4.x4x2x3x1", lambda_of_string "\\y1.\\y2.\\y3.\\y4.y1y2y3y4");;
let t8 = (lambda_of_string "\\x1.\\x2.x1x2", lambda_of_string "\\y1.\\y2.y2y1");;

(*tester_eq t1;
tester_eq t2;
tester_eq t3;
tester_eq t4;
tester_eq t5;
tester_eq t6;
tester_eq t7;*)
tester_eq t8;

