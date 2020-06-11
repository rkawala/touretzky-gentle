(in-package #:touretzky)

(defparameter *generator-lambda* (lambda () (random 6)))

(labels ((throw-die () (funcall *generator-lambda*))
         (throw-dice () (list (throw-die) (throw-die))))
  (macrolet ((set-rolls (rolls-arg &rest body) `(let* ((rolls ,rolls-arg) (*generator-lambda* (lambda () (pop rolls)))) ,@body)))
    (deftest ex5.6
     (testing "throw-die"
       (set-rolls '(2 5)
                  (ok (= 2 (length rolls)))
                  (ok (= 2 (throw-die)))
                  (ok (= 5 (throw-die)))
                  (ok (zerop (length rolls)))
                  (ok (null (throw-die)))))
     (testing "throw-dice"
       (set-rolls '(4 3)
                  (ok (equal '(4 3) (throw-dice))))))))
