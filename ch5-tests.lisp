(in-package #:touretzky)

(defparameter *generator-lambda* (lambda () (random 6)))

(labels ((throw-die () (funcall *generator-lambda*))
         (throw-dice () (list (throw-die) (throw-die))))
  (deftest ex5.6
    (testing "throw-die"
      (let* ((rolls '(2 5)) (*generator-lambda* (lambda () (pop rolls))))
        (ok (= 2 (length rolls)))
        (ok (= 2 (throw-die)))
        (ok (= 5 (throw-die)))
        (ok (zerop (length rolls)))
        (ok (null (throw-die)))))
    (testing "throw-dice"
      (let* ((rolls '(4 3)) (*generator-lambda* (lambda () (pop rolls))))
        (ok (equal '(4 3) (throw-dice)))))))
