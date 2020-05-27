(in-package #:touretzky)

(alexandria:define-constant honk-list '(honk if you like geese) :test #'equal)

(deftest ch2-7
  (testing "long input"
    (flet ((my-second (x) (first (rest x))))
      (ok (eq 'if (my-second honk-list))))))

(deftest ch2-8
  (testing "my-third using first and two rests"
    (flet ((my-third (x) (first (rest (rest x)))))
      (ok (eq 'you (my-third honk-list))))))

(deftest ch2-9
  (testing "my-third using second"
    (flet ((my-third (x) (second (rest x))))
      (ok (eq 'you (my-third honk-list))))))

(deftest ch2-12
  (testing "c...r for fourth position"
    (ok (eq 'like (cadddr honk-list)))))

(deftest ch2-13
    (testing "fun in the sun"
             (let ((a1 '(((fun)) (in the) (sun))))
               (ok (eq 'fun (caaar a1)))
               (ok (eq 'in (caadr a1)))
               (ok (eq 'the (cadadr a1)))
               (ok (eq 'sun (caaddr a1))))))

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
