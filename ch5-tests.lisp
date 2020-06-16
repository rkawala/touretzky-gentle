(in-package #:touretzky)

(defparameter *generator-lambda* (lambda () (random 6)))

(labels ((throw-die () (funcall *generator-lambda*))
         (throw-dice () (list (throw-die) (throw-die)))
         (snake-eyes-p (dice) (equal dice '(1 1)))
         (boxcars-p (dice) (equal dice '(6 6)))
         (instant-win-p (dice) (roll-has-value dice '(7 11)))
         (instant-loss-p (dice) (roll-has-value dice '(2 3 12)))
         (roll-has-value (roll valid-rolls) (member (reduce #'+ roll) valid-rolls)))
  (macrolet ((set-rolls (rolls-arg &rest body) `(let* ((rolls ,rolls-arg) (*generator-lambda* (lambda () (pop rolls)))) ,@body)))
    (labels ((iterate-all-rolls (valid-rolls predicate)
             (loop for i from 1 to 6
                   do (loop for j from 1 to 6
                            do (let* ((roll (list i j)) (rove-assert (if (roll-has-value roll valid-rolls) #'no-op #'not)))
                                 (ok (funcall rove-assert (funcall predicate roll)))))))
             
             (no-op (x) x))
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
                            (ok (equal '(4 3) (throw-dice)))))
               (testing "snake-eyes-p"
                 (iterate-all-rolls '(2) #'snake-eyes-p))
               (testing "boxcars-p"
                 (iterate-all-rolls '(12) #'boxcars-p))
               (testing "instant-win-p"
                 (iterate-all-rolls '(7 11) #'instant-win-p))
               (testing "instant-loss-p"
                 (iterate-all-rolls '(2 3 12) #'instant-loss-p))))))
