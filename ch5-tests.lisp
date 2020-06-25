(in-package #:touretzky)

(defparameter *generator-lambda* (lambda () (random 6)))

(defun instant-loss-p (dice) (roll-has-value dice '(2 3 12)))

(defun instant-win-p (dice) (roll-has-value dice '(7 11)))

(defun roll-has-value (roll valid-rolls) (member (value-of-roll roll) valid-rolls))

(defun value-of-roll (roll) (reduce #'+ roll))

(labels ((throw-die () (funcall *generator-lambda*))
         (throw-dice () (list (throw-die) (throw-die)))
         (snake-eyes-p (dice) (equal dice '(1 1)))
         (boxcars-p (dice) (equal dice '(6 6)))
         (say-throw (roll) (cond ((snake-eyes-p roll) 'snake-eyes) ((boxcars-p roll) 'boxcars) (t (value-of-roll roll))))
         (assemble-craps-string (roll) `((a) -- (b) -- ,(win-lose-or-nil roll)))
         (win-lose-or-nil (roll) (cond ((instant-win-p roll) '(you win)) ((instant-loss-p roll) '(you lose)))))
  (macrolet ((set-rolls (rolls-arg &rest body) `(let* ((rolls ,rolls-arg) (*generator-lambda* (lambda () (pop rolls)))) ,@body)))
    (labels ((iterate-all-rolls-with-list-of-true-rolls (valid-rolls predicate)
               (iterate-all-rolls predicate (lambda (roll predicate-result) (cond ((roll-has-value roll valid-rolls) (ok predicate-result)) (t (ng predicate-result))))))
             (iterate-all-rolls (predicate cond-lambda)
               (loop for i from 1 to 6
                   do (loop for j from 1 to 6
                            do (let* ((roll (list i j)) (predicate-result (funcall predicate roll)))
                                 (funcall cond-lambda roll predicate-result))))))
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
                        (iterate-all-rolls-with-list-of-true-rolls '(2) #'snake-eyes-p))
               (testing "boxcars-p"
                        (iterate-all-rolls-with-list-of-true-rolls '(12) #'boxcars-p))
               (testing "instant-win-p"
                        (iterate-all-rolls-with-list-of-true-rolls '(7 11) #'instant-win-p))
               (testing "instant-loss-p"
                        (iterate-all-rolls-with-list-of-true-rolls '(2 3 12) #'instant-loss-p))
               (testing "say-throw"
                 (iterate-all-rolls #'say-throw (lambda (roll predicate-result)
                                                  (cond ((equal roll '(1 1)) (ok (eq predicate-result 'snake-eyes)))
                                                        ((equal roll '(6 6)) (ok (eq predicate-result 'boxcars)))
                                                        (t (ok (= (value-of-roll roll) predicate-result)))))))
        (testing "assemble-craps-string clause 3"
          (mockingbird:with-dynamic-stubs ((instant-loss-p t))
            (ok (equal '(you lose) (car (nthcdr 4 (assemble-craps-string '(4 4)))))))
          (mockingbird:with-dynamic-stubs ((instant-win-p t))
            (ok (equal '(you win) (car (nthcdr 4 (assemble-craps-string '(4 4)))))))
          (ok (null (car (nthcdr 4 (assemble-craps-string '(4 4)))))))))))
