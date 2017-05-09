type algebraic_term = Var of string | Fun of string * (algebraic_term list)

(* Function gets system of equation as an input and returns
	function name that is never met in the system. The name
	is generated as concatenation of all names of 
	function. Maybe implementation should be changed *)
let get_fresh_fun_name system =

	(* Function with same meaning but for one algebraic term *)
	let rec get_fresh_name_at at =
		let rec impl at str = 
			match at with
				Var v -> v ^ str
				| Fun (f, atl) -> f ^ get_fresh_name_l atl in 
		impl at ""
	
	
	and get_fresh_name_l l =
		let rec impl l str =
			match l with
				[] -> str
				| (h :: t) -> (get_fresh_name_at h) ^ (impl t str) in
		impl l "" in

	(* Function gets system and string to concat with *)
	let rec impl system str =
		match system with
			[] -> str
			| (lhs, rhs)::t -> (get_fresh_name_at lhs) ^ (get_fresh_name_at rhs) ^ (impl t str) in 
	impl system "";;
			






let system_to_equation x = failwith "Not implemented";;
let apply_substitution x y = failwith "Not implemented";;
let check_solution x y = failwith "Not implemented";;
let solve_system x = failwith "Not implemented";;
