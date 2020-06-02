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