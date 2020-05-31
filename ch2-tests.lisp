(in-package #:touretzky)

(alexandria:define-constant honk-list '(honk if you like geese) :test #'equal)

(deftest ex2.7
  (testing "long input"
    (flet ((my-second (x) (first (rest x))))
      (ok (eq 'if (my-second honk-list))))))

(deftest ex2.8
  (testing "my-third using first and two rests"
    (flet ((my-third (x) (first (rest (rest x)))))
      (ok (eq 'you (my-third honk-list))))))

(deftest ex2.9
  (testing "my-third using second"
    (flet ((my-third (x) (second (rest x))))
      (ok (eq 'you (my-third honk-list))))))

(deftest ex2.12
  (testing "c...r for fourth position"
    (ok (eq 'like (cadddr honk-list)))))

(deftest ex2.13
    (testing "fun in the sun"
             (let ((a1 '(((fun)) (in the) (sun))))
               (ok (eq 'fun (caaar a1)))
               (ok (eq 'in (caadr a1)))
               (ok (eq 'the (cadadr a1)))
               (ok (eq 'sun (caaddr a1))))))

(deftest ex2.15
  (testing "more c...r"
    (let ((a1 '((a b) (c d) (e f))))
      (ok (equal '(a b) (car a1)))
      (ok (equal '((e f)) (cddr a1)))
      (ok (equal '(c d) (cadr a1)))
      (ok (equal '(b) (cdar a1)))
      (ok (eq 'b (cadar a1)))
      (ok (null (cddar a1)))
      (ok (eq 'a (caar a1)))
      (ok (equal '(f) (cdaddr a1)))
      (ok (eq 'f (car (cdaddr a1)))))))

(deftest sec2.10.4
  (testing "CAR and CDR of nil return nil rather than throwing an exception"
    (ok (null (car nil)))
    (ok (null (cdr nil)))))

(deftest ex2.17
  (testing "More CAR and CDRs"
    (ok (eq 'post (car '(post no bills))))
    (ok (equal '(no bills) (cdr '(post no bills))))
    (ok (equal '(post no) (car '((post no) bills))))
    (ok (null (cdr '(bills))))
    ;; (ok (signals (car 'bills)))  ; illegal, but that's what the book asked for
    (ok (equal '((no bills)) (cdr '(post (no bills)))))
    (ok (null (cdr '((post no bills)))))
    (ok (null (car nil)))))

(deftest sec2.11.1
  (testing "nil and cons"
    (ok (equal '(nil a b) (cons nil '(a b))))
    (ok (equal '((a b)) (cons '(a b) nil)))
    (ok (equal '(nil) (cons nil nil)))))

(deftest ex2.18
  (testing "given two inputs, create a list using cons"
    (flet ((cons-up-a-list (x y) (cons x (cons y nil))))
      (ok (equal '(foo bar) (cons-up-a-list 'foo 'bar))))))

(deftest ex2.19
  (testing "LIST and CONS results"
    (ok (equal '(fred and wilma) (list 'fred 'and 'wilma)))
    (ok (equal '(fred (and wilma)) (list 'fred '(and wilma))))
    (ok (equal '(fred and wilma) (cons 'fred '(and wilma))))
    (ok (equal '(nil) (cons nil nil)))
    (ok (equal '(nil nil) (list nil nil)))))

(deftest ex2.20
  (testing "More LIST and CONS"
    (ok (equal '(nil) (list nil)))
    (ok (equal '(t nil) (list t nil)))
    (ok (equal '(t) (cons t nil)))
    (ok (equal '((t)) (cons '(t) nil)))
    (ok (equal '((in one ear and) (out the other)) (list '(in one ear and) '(out the other))))
    (ok (equal '((in one ear and) out the other) (cons '(in one ear and) '(out the other))))))

(deftest ex2.21
  (testing "function that combines 4 scalars into two lists"
    (flet ((combine4 (e1 e2 e3 e4) (list (list e1 e2) (list e3 e4))))
      (ok (equal '((foo bar) (baz quux)) (combine4 'foo 'bar 'baz 'quux))))))

(deftest ex2.22
  (testing "duo-cons"
    (flet ((duo-cons (car1 car2 the-rest) (cons car1 (cons car2 the-rest))))
      (ok (equal '(patrick seymour marvin) (duo-cons 'patrick 'seymour '(marvin)))))))

(deftest ex2.23
  (testing "two-deeper"
    (flet ((two-deeper (x) (list (list x))))
      (ok (equal '((moo)) (two-deeper 'moo))))))

(deftest sec2.15
  (testing "nil is a list, but not a cons cell"
    (ok (equal t (listp nil)))
    (ok (null (consp nil)))))

(deftest ex2.29
  (testing "tally arithmetic: unary-add1"
    (flet ((unary-add1 (num) (cons 'x num)))
      (ok (equal '(x x x) (unary-add1 '(x x)))))))

(deftest ex2.31
  (testing "tally arithmetic: unary-zerop"
    (flet ((unary-zerop (num) (= 0 (length num))))
      (ok (unary-zerop nil))
      (ok (unary-zerop '()))
      (ok (not (unary-zerop '(x)))))))

(deftest ex2.32
  (testing "tally arithmetic: unary-greaterp"
    (flet ((unary-greaterp (left right) (> (length left) (length right))))
      (ok (not (unary-greaterp nil '())))
      (ok (not (unary-greaterp '(x x) '(x x x))))
      (ok (unary-greaterp '(x x x) '(x x)))
      (ok (unary-greaterp '(x) nil)))))

(deftest array-length
  (testing "array length"
    (let ((ary #(1 2 3 4 5)))
      (ok (= (length ary) 5))
      (ng (= (length ary) 3))
      (ok (signals (error "foo"))))))

(deftest array-position
  (testing "array position"
    (let ((ary #(1 2 3 4 5)))
      (ok (= (position 4 ary) 3))
      (ok (= (position-if #'evenp ary) 1))
      (ng (eql (position-if #'evenp #(1 3 5)) 1))
      (skip "okay"))))
