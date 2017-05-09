open Hw1

type simp_type = S_Elem of string | S_Arrow of simp_type * simp_type
val infer_simp_type : lambda -> ((string * simp_type list) * simp_type) option

type hm_lambda = HM_Var of string | HM_Abs of string * lambda | HM_App of lambda * lambda | HM_Let of string * lambda * lambda
type hm_type = HM_Elem of string | HM_Arrow of hm_type * hm_type | HM_ForAll of string * hm_type
val algorithm_w : hm_lambda -> ((string * hm_type list) * hm_type) option