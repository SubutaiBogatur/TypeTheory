
(* First part of homework was to write reverse function for custom list my_list*)
type 'a my_list = Nil
	| Cons of 'a * ('a my_list)

let print_int_my_list l = 
	let rec print_int_my_list_rec l =
		match l with
			Cons (h, t) -> print_int h; print_string " "; print_int_my_list_rec t
			| Nil -> () in
	print_int_my_list_rec l;
	print_string "\n";;

let reverse_my_list l =
	let rec reverse_impl old_list new_list =
		match old_list with
			Nil -> new_list
			| Cons(h, t) -> reverse_impl t (Cons(h, new_list)) in
	reverse_impl l Nil;; 

(*let tmp_list = Cons(3, Cons(5, Cons(10, Nil)));;
print_int_my_list tmp_list;;
print_int_my_list (reverse_my_list tmp_list)*)



(*Second part of homework was to write mergesort for default OCaml list*)

let rec print_list_int l =
	match l with
		[] -> ()
		| h::t -> print_int h; print_list_int t;;

let merge_sort_int l =
	(*Function splits list in two equal in length if even num of elements in l, else
		first returned in tuple list has one element more*)
	let split_in_two_equal l =
		let rec split_impl l_old l_new_1 l_new_2 =
			match l_old with
				h1::h2::t -> split_impl t (h1::l_new_1) (h2::l_new_2)
				| h1::[] -> ((h1::l_new_1), l_new_2)
				| [] -> (l_new_1, l_new_2) in
		split_impl l [] [] in

	let merge_two_sorted_lists l1 l2 =
		let rec merge_impl l1 l2 ans =
			match l1 with
				[] -> (match l2 with
					[] -> ans
					| h::t -> merge_impl l1 t (h::ans))
				| h::t -> (match l2 with
					[] -> merge_impl t l2 (h::ans)
| h2::t2 -> if h < h2 then merge_impl t l2 (h::ans) else merge_impl l1 t2 (h2::ans)) in

		let tup = merge_impl l1 l2 [] in
		List.rev tup in

	let rec merge_impl l =
		match l with
			[] -> l
			| h::[] -> l
			| h::t -> 
				(let tup = split_in_two_equal l in
				let (f, s) = tup in
				merge_two_sorted_lists (merge_impl f) (merge_impl s)) in
	merge_impl l;; 	


	
let l = [4;3;2;5;6;7;4;3;2;1];;
print_list_int (merge_sort_int l); print_string "\n";;

