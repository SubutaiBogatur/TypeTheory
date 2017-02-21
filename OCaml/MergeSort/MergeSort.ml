
let rev l =
	let rec rev_impl old_l new_l =
		match old_l with
			[] -> new_l
			| h::t -> rev_impl t (h::new_l) in
	rev_impl l [];; 

let rec print_list_int l =
	match l with
		[] -> print_string "\n"; ()
                | h::t -> print_int h; print_list_int t;;

let merge_sort l =
	let split_in_two_equal l =
		let rec split_impl l_old l_new_1 l_new_2 =
			match l_old with
				h1::h2::t -> split_impl t (h1::l_new_1) (h2::l_new_2)
				| h1::[] -> ((h1::l_new_1), l_new_2)
				| [] -> (l_new_1, l_new_2) in
		split_impl l [] [] in

	let merge_two_sorted_lists l1 l2 =
		let rec merge_impl l1 l2 ans =
			match (l1, l2) with
				([], []) -> ans
				| ([], h::t) -> merge_impl l1 t (h::ans)
				| (h::t, []) -> merge_impl t l2 (h::ans)
				| (h1::t1, h2::t2) -> if h1 < h2 then merge_impl t1 l2 (h1::ans) else merge_impl l1 t2 (h2::ans) in
		let tup = merge_impl l1 l2 [] in
		rev tup in

	let rec merge_impl l =
		match l with
			[] -> l
			| h::[] -> l
			| h::t -> 
				(let tup = split_in_two_equal l in
				let (f, s) = tup in
				merge_two_sorted_lists (merge_impl f) (merge_impl s)) in
	merge_impl l;; 	
