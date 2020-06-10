(in-package #:touretzky)

(defparameter *generator-lambda* (lambda () (random 6)))

(labels ((throw-die () (funcall *generator-lambda*)))
  (deftest ex5.6
    (testing "throw-die"
      (let* ((rolls '(2 5)) (*generator-lambda* (lambda () (pop rolls))))
        (ok (= 2 (length rolls)))
        (ok (= 2 (throw-die)))
        (ok (= 5 (throw-die)))
        (ok (zerop (length rolls)))
        (ok (null (throw-die)))))))
