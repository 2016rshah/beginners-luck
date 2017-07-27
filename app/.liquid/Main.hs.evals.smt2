(set-option :auto-config false)
(set-option :model true)
(set-option :model.partial false)

(set-option :smt.mbqi false)
(define-sort Str () Int)
(declare-fun strLen (Str) Int)
(declare-fun subString (Str Int Int) Str)
(declare-fun concatString (Str Str) Str)
(define-sort Elt () Int)
(define-sort Set () (Array Elt Bool))
(define-fun smt_set_emp () Set ((as const Set) false))
(define-fun smt_set_mem ((x Elt) (s Set)) Bool (select s x))
(define-fun smt_set_add ((s Set) (x Elt)) Set (store s x true))
(define-fun smt_set_cup ((s1 Set) (s2 Set)) Set ((_ map or) s1 s2))
(define-fun smt_set_cap ((s1 Set) (s2 Set)) Set ((_ map and) s1 s2))
(define-fun smt_set_com ((s Set)) Set ((_ map not) s))
(define-fun smt_set_dif ((s1 Set) (s2 Set)) Set (smt_set_cap s1 (smt_set_com s2)))
(define-fun smt_set_sub ((s1 Set) (s2 Set)) Bool (= smt_set_emp (smt_set_dif s1 s2)))
(define-sort Map () (Array Elt Elt))
(define-fun smt_map_sel ((m Map) (k Elt)) Elt (select m k))
(define-fun smt_map_sto ((m Map) (k Elt) (v Elt)) Map (store m k v))
(define-fun bool_to_int ((b Bool)) Int (ite b 1 0))
(define-fun Z3_OP_MUL ((x Int) (y Int)) Int (* x y))
(define-fun Z3_OP_DIV ((x Int) (y Int)) Int (div x y))
(declare-fun int_apply_$35$$35$3 (Int Int Int Int) Int)
(declare-fun bool_apply_$35$$35$6 (Int Int Int Int Int Int Int) Bool)
(declare-fun map_apply_$35$$35$4 (Int Int Int Int Int) Map)
(declare-fun real_apply_$35$$35$2 (Int Int Int) Real)
(declare-fun lam_int_arg$35$$35$3 () Int)
(declare-fun set_apply_$35$$35$1 (Int Int) Set)
(declare-fun lam_int_arg$35$$35$5 () Int)
(declare-fun set_to_int (Set) Int)
(declare-fun bitvec_apply$35$$35$6 (Int Int Int Int Int Int Int) (_ BitVec 32))
(declare-fun int_apply_$35$$35$5 (Int Int Int Int Int Int) Int)
(declare-fun map_apply_$35$$35$2 (Int Int Int) Map)
(declare-fun real_apply_$35$$35$4 (Int Int Int Int Int) Real)
(declare-fun lam_int_arg$35$$35$2 () Int)
(declare-fun bitvec_apply$35$$35$1 (Int Int) (_ BitVec 32))
(declare-fun int_apply_$35$$35$2 (Int Int Int) Int)
(declare-fun bool_apply_$35$$35$7 (Int Int Int Int Int Int Int Int) Bool)
(declare-fun map_apply_$35$$35$5 (Int Int Int Int Int Int) Map)
(declare-fun real_apply_$35$$35$3 (Int Int Int Int) Real)
(declare-fun set_apply_$35$$35$6 (Int Int Int Int Int Int Int) Set)
(declare-fun bitvec_apply$35$$35$7 (Int Int Int Int Int Int Int Int) (_ BitVec 32))
(declare-fun int_apply_$35$$35$4 (Int Int Int Int Int) Int)
(declare-fun bool_apply_$35$$35$1 (Int Int) Bool)
(declare-fun map_apply_$35$$35$3 (Int Int Int Int) Map)
(declare-fun real_apply_$35$$35$5 (Int Int Int Int Int Int) Real)
(declare-fun lam_int_arg$35$$35$4 () Int)
(declare-fun lam_int_arg$35$$35$1 () Int)
(declare-fun bitvec_apply$35$$35$2 (Int Int Int) (_ BitVec 32))
(declare-fun int_apply_$35$$35$1 (Int Int) Int)
(declare-fun bool_apply_$35$$35$4 (Int Int Int Int Int) Bool)
(declare-fun map_apply_$35$$35$6 (Int Int Int Int Int Int Int) Map)
(declare-fun set_apply_$35$$35$7 (Int Int Int Int Int Int Int Int) Set)
(declare-fun map_to_int (Map) Int)
(declare-fun set_apply_$35$$35$2 (Int Int Int) Set)
(declare-fun real_apply_$35$$35$1 (Int Int) Real)
(declare-fun bitvec_to_int ((_ BitVec 32)) Int)
(declare-fun bitvec_apply$35$$35$3 (Int Int Int Int) (_ BitVec 32))
(declare-fun bool_apply_$35$$35$5 (Int Int Int Int Int Int) Bool)
(declare-fun map_apply_$35$$35$7 (Int Int Int Int Int Int Int Int) Map)
(declare-fun set_apply_$35$$35$4 (Int Int Int Int Int) Set)
(declare-fun real_to_int (Real) Int)
(declare-fun smt_lambda (Int Int) Int)
(declare-fun set_apply_$35$$35$3 (Int Int Int Int) Set)
(declare-fun lam_int_arg$35$$35$7 () Int)
(declare-fun int_apply_$35$$35$7 (Int Int Int Int Int Int Int Int) Int)
(declare-fun bitvec_apply$35$$35$4 (Int Int Int Int Int) (_ BitVec 32))
(declare-fun bool_apply_$35$$35$2 (Int Int Int) Bool)
(declare-fun real_apply_$35$$35$6 (Int Int Int Int Int Int Int) Real)
(declare-fun set_apply_$35$$35$5 (Int Int Int Int Int Int) Set)
(declare-fun bool_apply_$35$$35$3 (Int Int Int Int) Bool)
(declare-fun map_apply_$35$$35$1 (Int Int) Map)
(declare-fun real_apply_$35$$35$7 (Int Int Int Int Int Int Int Int) Real)
(declare-fun lam_int_arg$35$$35$6 () Int)
(declare-fun int_apply_$35$$35$6 (Int Int Int Int Int Int Int) Int)
(declare-fun bitvec_apply$35$$35$5 (Int Int Int Int Int Int) (_ BitVec 32))
(push 1)