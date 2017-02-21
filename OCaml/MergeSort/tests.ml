open MergeSort;;

let l0 = [1;2;3;4;5;6;7;8;9;10;11];;
let l1 = [4;3;2;1;7;5;8;6;4;9;7;5;4;2;3;4;6;7];;
let l2 = [3;6;9;1;6;2;8;4;6;7;9;4];;
let l3 = [99;66;33;22;666;8;1;4;7;9;0];;

print_list_int l0;;
print_list_int (rev l0);;
print_list_int (merge_sort l0);;
print_list_int (merge_sort l1);;
print_list_int (merge_sort l2);;
print_list_int (merge_sort l3);;
