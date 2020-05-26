(in-package #:touretzky)

(deftest ch2-13
    (testing "fun in the sun"
             (let ((a1 '(((fun) (in the) (sun)))))
               (ok (eq 'fun (caaar a1)))
               (ok (eq 'in (caadar a1)))
               (ok (eq 'the (car (cdadar a1))))
               (ok (eq 'sun (car (caddar a1)))))))

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
