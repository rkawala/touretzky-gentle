(in-package #:touretzky)

(defun rick (x)
  (+ x 1))

(deftest array-length
  (testing "array length"
    (let ((ary #(1 2 3 4 5)))
      (ok (= (length ary) 5))
      (ng (= (length ary) 3))
      (ok (signals (length 1))))))

(deftest array-position
  (testing "array position"
    (let ((ary #(1 2 3 4 5)))
      (ok (= (position 4 ary) 3))
      (ok (= (position-if #'evenp ary) 1))
      (ng (eql (position-if #'evenp #(1 3 5)) 1))
      (skip "okay"))))
