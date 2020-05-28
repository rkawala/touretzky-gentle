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
