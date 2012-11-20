(defpackage tigress
  (:use common-lisp))

(in-package :tigress)



;; list a = nil | cons a (list a)   ;; defines nil, cons, case
;; xs == ys = ... Nil ... Cons ...
;; reverse xs = ... Nil ... Cons ...
;; isPalin xs = xs == reverse xs

;; (list nil cons case)  ;; relate nil, cons, case as an instance of a list abstraction
;;                       ;; (perhaps (list :nil nil :cons cons :case case))

;; (=> (list nil cons case) (eq ==))  ;; for any list, there is an instance of an == abstraction

;; (=> (list nil cons case) (reverse reverse)) ;; for any list, there is an instance of a reverse abstraction

;; (=> (eq ==) (reverse reverse) (isPalin isPalin)) ;; for any eq and reverse instances, there is an isPalin instance

;; But how do we know that eq and reverse are related at the same point.  There is no way for the solver to
;; connect those, so it might pick an eq on int and a reverse on Map, and end up with nonsense.

;; Of course, if types are in the game...

;; (forall (A) (list listA A nil cons case))   ;; list is a polymorphic instance of a list abstraction

;; (=> (list listA A nil cons case) (eq a ==) (eq list ==*)) ;; if list is an instance of list, and == is an instance of eq, then list is also an instance of eq

;; (=> (list listA A nil cons case) (reverse listA reverse))

;; (=> (eq A ==) (reverse A reverse) (isPalin A isPalin))

