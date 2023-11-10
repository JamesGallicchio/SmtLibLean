(set-logic QF_LIRA)
(declare-const x Int)
(declare-const y Real)
(assert
    (and
        (>= x (* 3 y))
        (<= x y)
        (< (- 2) x)
    )
)
(assert (not (<= (- y x) (/ 2 3))))
(check-sat)
