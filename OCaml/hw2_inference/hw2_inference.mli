open Hw1
open Hw1_reduction

type simp_type = S_Elem of string | S_Arrow of simp_type * simp_type
val infer_simp_type : lambda -> ((string * simp_type) list * simp_type) option

type hm_lambda = HM_Var of string | HM_Abs of string * hm_lambda | HM_App of hm_lambda * hm_lambda | HM_Let of string * hm_lambda * hm_lambda
type hm_type = HM_Elem of string | HM_Arrow of hm_type * hm_type | HM_ForAll of string * hm_type

val string_of_hml : hm_lambda -> string
val string_of_hmt : hm_type -> string

val algorithm_w : hm_lambda -> ((string * hm_type) list * hm_type) option
