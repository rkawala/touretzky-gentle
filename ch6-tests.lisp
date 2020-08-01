(in-package #:touretzky)

;;; Exercise 6.24 shows another way to test for set equality, but I
;;; like mine too. It's less elegant, but it's easier to understand.
(defun equal-unordered (expected actual) (equal expected (sort actual #'string-lessp)))

(deftest early-ch6
  (testing "dot notation for car"
    (ok (equal '(w x y z) '(w . (x y z)))))
  (testing "cons to the front"
    (ok (equal '(w x y z) (cons 'w '(x y z)))))
  (testing "and cons to the end (which doesn't append)"
    (ok (equal '((w x y) . z) (cons '(w x y) 'z)))))

(deftest append
  (testing "append hooks two lists together"
    (ok (equal '(a b c d) (append '(a b) '(c d)))))
  (testing "if the first list is empty, you just get the second list"
    (ok (equal '(a b) (append '() '(a b))))
    (ok (equal '(a b) (append nil '(a b)))))
  (testing "if the second list is empty, you just get the first list"
    (ok (equal '(a b) (append '(a b) '())))
    (ok (equal '(a b) (append '(a b) nil))))
  (testing "appending an empty list to an empty list gets you a single empty list"
    (ok (equal nil (append '() '())))
    (ok (equal '() (append nil nil))))
  (testing "append re-conses the first list but not the second"
    (let* ((l1 '(a b))
           (l2 '(c d))
           (both (append l1 l2))
           (first-cons (nthcdr 0 both))
           (second-cons (nthcdr 1 both))
           (third-cons (nthcdr 2 both))
           (fourth-cons (nthcdr 3 both)))
      (ng (eq l1 first-cons))
      (ng (eq (cdr l1) second-cons))
      (ok (eq both first-cons))
      (ok (eq (cdr both) second-cons))
      (ok (eq l2 third-cons))
      (ok (eq (cdr l2) fourth-cons))))
  (testing "append takes an atom as the second argument, but the first arg must be a list"
    (ok (equal '(a . b) (append '(a) 'b))))
  (testing "append can be implemented with two reversals"
    (ok (equal '(a b c) (reverse (cons 'c (reverse '(a b))))))))

(deftest first-vs-last
  (testing "first returns the car of the first cons of a list"
    (ok (eq 'a (first '(a b c)))))
  (testing "last returns the entire cons at the end of a list, not the car"
    (let* ((final-cons (cons 'c nil)) (a-list (append '(a b) final-cons)))
      (ok (eq 'a (first a-list)))
      (ok (eq final-cons (last a-list))))))

(deftest ch6-exercises-part-1
   (testing "ex6.2 (nth returns the car...)"
     (ok (signals (nth 3 '(a b c . d)) 'type-error)))
  (testing "ex6.3 (... but last returns the entire cons cell)"
    (ok (equal '(rosebud) (last '(rosebud)))))
  (testing "ex6.4"
    (ok (equal '((a b c)) (last '((a b c))))))
  (testing "ex6.5"
    (let ((line '(roses are red)))
      (ok (equal '(red are roses) (reverse line)))
      (ok (eq 'red (first (last line))))
      (ok (eq 'are (nth 1 line)))
      (ok (equal line (reverse (reverse line))))
      (ok (equal '(roses are red roses) (append line (list (first line)))))
      (ok (equal '(red roses are red) (append (last line) line)))
      (ok (equal '(roses (red)) (list (first line) (last line))))
      (ok (equal '((red) roses are red) (cons (last line) line)))
      (ok (equal '(roses red) (remove 'are line)))
      (ok (equal '(roses are red violets are blue) (append line '(violets are blue))))))
  (testing "ex6.6"
    (flet ((last-element (lst) (car (last lst)))
           (last-element-with-reverse (lst) (car (reverse lst)))
           (last-element-with-nth (lst) (nth (1- (length lst)) lst)))
      (ok (eq 'c (last-element '(a b c))))
      (ok (eq 'c (last-element-with-reverse '(a b c))))
      (ok (eq 'c (last-element-with-nth '(a b c))))))
  (testing "ex6.7"
    (flet ((next-to-last-with-reverse (lst) (cadr (reverse lst)))
           (next-to-last-with-nth (lst) (nth (- (length lst) 2) lst)))
      (ok (eq 'c (next-to-last-with-reverse '(a b c d))))
      (ok (eq 'c (next-to-last-with-nth '(a b c d))))))
  (testing "ex6.8"
    (flet ((my-penultimate (lst) (reverse (cdr (reverse lst)))))
      (ok (equal '(a b) (my-penultimate '(a b c))))))
  (testing "ex6.10"
    (flet ((palindromep (lst) (equal lst (reverse lst))))
      (ok (palindromep '()))
      (ok (palindromep '(a)))
      (ok (palindromep '(a b c c b a)))
      (ng (palindromep '(a b)))))
  (testing "ex6.11"
    (flet ((make-palindrome (lst) (append lst (reverse lst))))
      (ok (equal '(a b c c b a) (make-palindrome '(a b c))))))
  (testing "ex6.13"
    (ok (null (intersection '(a b) nil))))
  (testing "ex6.14 (sort needed because intersection rearranges the list)"
    (let ((my-set '(a b c)))
      (ok (equal-unordered my-set (intersection my-set my-set)))))
  (testing "ex6.16"
    (ok (equal-unordered '(a b c) (union nil '(a b c)))))
  (testing "ex6.17"
    (let ((my-set '(a b c)))
      (ok (equal-unordered my-set (union my-set my-set)))))
  (testing "ex6.18"
    (flet ((add-vowels (lst) (union lst '(a e i o u))))
      (ok (equal-unordered '(a b c e i o u) (add-vowels '(a b c))))))
  (testing "ex6.19"
    (ok (null (set-difference nil '(a b c))))
    (ok (equal '(a b c) (set-difference '(a b c) nil))))
  (testing "ex6.21"
    (flet ((my-subsetp (small big)
             (null (set-difference small big))
;;;          My first try was:             
;;;          (eql (length (set-difference big small)) (- (length big) (length small)))
             ))
      (ok (null (my-subsetp '(a d) '(a b c))))
      (ok (eq t (my-subsetp '(a b d) '(a b c d))))))
  (testing "ex6.22"
    (let ((a '(soap water)))
      (ok (equal-unordered '(no radio soap water) (union a '(no soap radio))))
      (ok (equal-unordered '(soap water) (intersection a (reverse a))))
      (ok (equal '(soap) (set-difference a '(stop for water))))
      (ok (null (set-difference a a)))
      (ok (equal a (member 'soap a)))
      (ok (equal '(water) (member 'water a)))
      (ok (null (member 'washcloth a)))))
  (testing "ex6.24"
    (flet ((set-equal (lst1 lst2) (and (subsetp lst1 lst2) (subsetp lst2 lst1))))
      (ok (set-equal '(a b) '(b a)))
      (ok (set-equal nil nil))
      (ng (set-equal '(a) '(b)))))
  (testing "ex6.25"
    (flet ((proper-subsetp (lst1 lst2) (and (subsetp lst1 lst2) (not (subsetp lst2 lst1)))))
      (ok (proper-subsetp '() '(a)))
      (ok (proper-subsetp '(a b) '(a b c d)))
      (ng (proper-subsetp '(a b) '(b)))
      (ng (proper-subsetp '(a b) '(a b)))
      (ng (proper-subsetp '() '())))))

;;; I'm not worrying about detecting that the separator might be missing.
;;; This is a student exercise, not production-quality code.
(deftest ex6.26
  (let ((separator '-vs-))
    (labels ((make-vs-list (lst1 lst2) (append lst1 (list separator) lst2))
             (right-side (lst) (cdr (member separator lst)))
             (left-side (lst) (if (eq separator (car lst)) nil (cons (car lst) (left-side (cdr lst)))))
             (count-common (left right) (length (intersection left right)))
             (compare (lst) (list (count-common (left-side lst) (right-side lst)) 'common 'features)))
      (testing "right-side"
        (ok (equal '(d e) (right-side (make-vs-list '(a b c) '(d e))))))
      (testing "left-side"
        (ok (equal '(a b c) (left-side (make-vs-list '(a b c) '(d e))))))
      (testing "count-common"
        (ok (eq 3 (count-common '(a b c d e) '(c d e f g)))))
      (testing "compare"
        (ok (equal '(3 common features) (compare '(a b c d e -vs- c d e f g))))))))

(deftest ch6-exercises-the-rest
  (testing "ex6.28"
    (let ((produce '((apple . fruit) (celery . veggie) (banana . fruit) (lettuce . veggie))))
      (ok (equal '(banana . fruit) (assoc 'banana produce)))
      (ok (equal '(apple . fruit) (rassoc 'fruit produce)))
      (ok (equal '(lettuce . veggie) (assoc 'lettuce produce)))
      (ok (equal '(celery . veggie) (rassoc 'veggie produce)))))
  (testing "ex6.30"
           (setf *print-circle* t)
           (let ((nerd-states '#0=(sleeping eating waiting-for-a-computer programming debugging . #0#)))
             (labels ((nerdus (current-state)
                        (cadr (find-state current-state nerd-states)))
                      (sleepless (current-state)
                        (let ((next-state (cdr (find-state current-state nerd-states))))
                          (if (eq 'sleeping (car next-state)) (cadr next-state) (car next-state))))
                      (nerd-on-caffeine (current-state)
                        (caddr (find-state current-state nerd-states)))
                      (find-state (state state-list) (find-state-internal state state-list state-list))
                      (find-state-internal (state current-state beginning)
                         (cond ((eq state (car current-state)) current-state)
                               ((eq (cdr current-state) beginning) nil)
                               (t (find-state-internal state (cdr current-state) beginning)))))
               (ok (eq 'debugging (car (find-state 'debugging nerd-states))))
               (ok (null (find-state 'no-such-state nerd-states)))
               (ok (eq 'eating (nerdus 'sleeping)))
               (ok (eq 'sleeping (nerdus 'debugging)))
               (ok (eq 'eating (sleepless 'debugging)))
               (ok (eq 'waiting-for-a-computer (sleepless 'eating)))
               (ok (eq 'programming (nerd-on-caffeine 'eating))))))
  ;; The solution in the back of the book doesn't work for lists shorter
  ;; than two elements. This version fixes that.
  (testing "ex6.36"
    (flet ((swap-first-last (lst)
             (let* ((a (reverse (cdr lst))))
               (if a
                   (cons (first a) (append (reverse (cdr a)) (list (first lst))))
                   lst))))
      (ok (null (swap-first-last '())))
      (ok (equal '(one-element) (swap-first-last '(one-element))))
      (ok (equal '(e2 e1) (swap-first-last '(e1 e2))))
      (ok (equal '(e3 e2 e1) (swap-first-last '(e1 e2 e3))))
      (ok (equal '(love cant buy you) (swap-first-last '(you cant buy love))))))
  (testing "ex6.37"
    (let ((x '(a b c d e)))
      (flet ((rotate-left (lst) (append (cdr lst) (list (car lst))))
             (rotate-right (lst) (append (last lst) (reverse (cdr (reverse lst))))))
        (ok (equal '(b c d e a) (rotate-left x)))
        (ok (equal '(e a b c d) (rotate-right x))))))
  (testing "ex6.40"
    (labels ((member-equiv (lst)
               (cond ((null lst) '())
                     (t (cons (list (car lst) lst) (member-equiv (cdr lst))))))
             (do-assert (sym expected)
               (ok (equal expected (cadr (assoc sym (member-equiv '(a b c d))))))))
      (do-assert 'e nil)
      (do-assert 'd '(d))
      (do-assert 'c '(c d))
      (do-assert 'b '(b c d))
      (do-assert 'a '(a b c d)))))
