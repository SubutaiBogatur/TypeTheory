open LambdaParser;;

lambda_of_string "\\x.\\y.xy";;
lambda_of_string "xy";;
lambda_of_string "(x)";;
(*lambda_of_string "()";;*)
lambda_of_string "(((((((\\y.y)))))))";;
lambda_of_string "((z))(\\x.\\y.((xy)))";;
lambda_of_string "\\x.\\y.xy";;
lambda_of_string "\\x.\\y.xy";;


(*
print_string (Hw1.string_of_lambda (Hw1.lambda_of_string "\\x.\\y.x"));;
*)
