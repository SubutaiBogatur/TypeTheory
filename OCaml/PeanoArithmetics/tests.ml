open PeanoArithmetics;;

print_int (int_of_peano (S (S (Z)))); print_string "\n";;
print_int (int_of_peano Z); print_string "\n";;
print_int (int_of_peano (add (peano_of_int 5) (peano_of_int 7))); print_string "\n";;
print_int (int_of_peano (mul (peano_of_int 5) (peano_of_int 7))); print_string "\n";;
print_int (int_of_peano (power (peano_of_int 5) (peano_of_int 7))); print_string "\n";;
print_int (int_of_peano (sub (peano_of_int 26) (peano_of_int 14))); print_string "\n";;
print_int (int_of_peano (sub (peano_of_int 5) (peano_of_int 7))); print_string "\n";;
print_int (int_of_peano (inc (peano_of_int 5))); print_string "\n";;

