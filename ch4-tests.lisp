(in-package #:touretzky)

(deftest ex4.1-4
    (testing "make-even"
             (flet ((make-even (x) (if (evenp x) x (+ x 1))))
               (ok (= 2 (make-even 1)))
               (ok (= 2 (make-even 2)))
               (ok (= -2 (make-even -3)))))
  (testing "further"
    (flet ((further (x) (if (evenp x) x (+ x (if (minusp x) -1 1)))))
      (ok (= 2 (further 1)))
      (ok (= 2 (further 2)))
      (ok (= -4 (further -3)))))

  (testing "my-not"
    (flet ((my-not (x) (if x nil t)))
      (ok (eq t (my-not nil)))
      (ok (eq nil (my-not t)))
      (ok (eq t (my-not '())))))

  (testing "ordered"
    (flet ((ordered (x y) (if (> x y) (list y x) (list x y))))
      (ok (equal '(3 4) (ordered 3 4)))
      (ok (equal '(3 4) (ordered 4 3))))))

(deftest ex4.6
  (testing "my-abs using cond"
    (flet ((my-abs (x) (cond ((< x 0) (- x)) (t x))))
      (ok (= 1 (my-abs -1)))
      (ok (= 0 (my-abs 0)))
      (ok (= 1 (my-abs 1))))))

(deftest ex4.10
  (testing "constrain with cond"
    (flet ((constrain (x min max) (cond ((< x min) min) ((> x max) max) (t x))))
      (ok (= 2 (constrain 1 2 3)))
      (ok (= 3 (constrain 50 2 3)))
      (ok (= 199 (constrain 199 0 299)))))
  (testing "constrain with if"
    (flet ((constrain (x min max) (if (< x min) min (if (> x max) max x))))
      (ok (= 2 (constrain 1 2 3)))
      (ok (= 3 (constrain 50 2 3)))
      (ok (= 199 (constrain 199 0 299))))))

(deftest ex4.11
  (testing "firstzero"
    (labels ((check-digit (number-list symbol-list)
               (cond ((= 0 (car number-list)) (car symbol-list))
                     ((null (cdr number-list)) 'none)
                     (t (check-digit (cdr number-list) (cdr symbol-list)))))
             (firstzero (number-list) (check-digit number-list '(first second third))))
      (ok (eq 'first (firstzero '(0 1 2))))
      (ok (eq 'second (firstzero '(-1 0 1))))
      (ok (eq 'third (firstzero '(-2 -1 0))))
      (ok (eq 'none (firstzero '(1 2 3)))))))

(deftest ex4.13
  (testing "how-compute"
    (flet ((how-compute (x1 x2 answer)
             (cond ((= answer (+ x1 x2)) 'sum-of)
                   ((= answer (* x1 x2)) 'product-of)
                   (t '(beats me)))))
      (ok (eq 'sum-of (how-compute 44 2 46)))
      (ok (eq 'product-of (how-compute 44 2 88)))
      (ok (equal '(beats me) (how-compute 23 45 111))))))

(deftest ex4.14
  (testing "return values from AND (first nil arg, or last non-nil arg) & OR (first non-nil arg, or nil)"
    (ok (eq 'foe (and 'fee 'fie 'foe)))
    (ok (eq 'fee (or 'fee 'fie 'foe)))
    (ok (eq 'foe (or nil 'foe nil)))
    (ok (null (and 'fee 'fie nil)))
    (ok (eq 'yes (and (equal 'abc 'abc) 'yes)))
    (ok (eq t (or (equal 'abc 'abc) 'yes)))))


(deftest ex4.15
  (testing "geqp"
    (flet ((geqp (x y) (>= x y)))
      (ok (geqp 2 1))
      (ng (geqp 1 2)))))

(deftest ex4.16
  (testing "by-two"
    (flet ((by-two (x) (cond ((and (oddp x) (> x 0)) (* x x))
                             ((and (oddp x) (minusp x)) (* x 2))
                             (t (/ x 2)))))
      (ok (= 9 (by-two 3)))
      (ok (= -6 (by-two -3)))
      (ok (= 2 (by-two 4))))))

(deftest ex4.17
  (testing "sex-age-p"
    (flet ((sex-age-p (sex age) (or
                                 (and (eq age 'child) (or (eq sex 'boy) (eq sex 'girl)))
                                 (and (eq age 'adult) (or (eq sex 'man) (eq sex 'woman))))))
      (ok (sex-age-p 'boy 'child))
      (ok (sex-age-p 'girl 'child))
      (ok (sex-age-p 'man 'adult))
      (ok (sex-age-p 'woman 'adult))
      (ng (sex-age-p 'boy 'adult))
      (ng (sex-age-p 'woman 'child)))))

(deftest ex4.18
  (testing "rochambeau"
    (labels ((rochambeau (player1-pick player2-pick)
               (cond ((left-wins player1-pick player2-pick) 'first-wins)
                     ((left-wins player2-pick player1-pick) 'second-wins)
                     (t 'tie)))
             (left-wins (left right)
               (or
                 (combo left right 'rock 'scissors)
                 (combo left right 'scissors 'paper)
                 (combo left right 'paper 'rock)))
             (combo (left right pick1 pick2) (and (eq left pick1) (eq right pick2))))
      (ok (eq 'tie (rochambeau 'rock 'rock)))
      (ok (eq 'first-wins (rochambeau 'rock 'scissors)))
      (ok (eq 'second-wins (rochambeau 'rock 'paper)))
      (ok (eq 'second-wins (rochambeau 'scissors 'rock)))
      (ok (eq 'tie (rochambeau 'scissors 'scissors)))
      (ok (eq 'first-wins (rochambeau 'scissors 'paper)))
      (ok (eq 'first-wins (rochambeau 'paper 'rock)))
      (ok (eq 'second-wins (rochambeau 'paper 'scissors)))
      (ok (eq 'tie (rochambeau 'paper 'paper))))))

(deftest ex4.19
  (testing "Do AND using COND"
    (flet ((and-with-cond (x y z)
             (cond ((not x) nil)
                   ((not y) nil)
                   ((not z) nil)
                   (t z))))
      (ng (and-with-cond nil nil nil))
      (ng (and-with-cond nil nil 'z))
      (ng (and-with-cond nil 'y nil))
      (ng (and-with-cond nil 'y 'z))
      (ng (and-with-cond 'x nil nil))
      (ng (and-with-cond 'x nil 'z))
      (ng (and-with-cond 'x 'y nil))
      (ok (eq 'z (and-with-cond 'x 'y 'z)))))
  (testing "Do AND using IF"
    (flet ((and-with-if (x y z)
             (if (not x) nil
                 (if (not y) nil
                     (if (not z) nil
                         z)))))
      (ng (and-with-if nil nil nil))
      (ng (and-with-if nil nil 'z))
      (ng (and-with-if nil 'y nil))
      (ng (and-with-if nil 'y 'z))
      (ng (and-with-if 'x nil nil))
      (ng (and-with-if 'x nil 'z))
      (ng (and-with-if 'x 'y nil))
      (ok (eq 'z (and-with-if 'x 'y 'z))))))

