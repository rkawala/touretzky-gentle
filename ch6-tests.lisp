(in-package #:touretzky)

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

(deftest simple
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
      (ok (equal '(a b c c b a) (make-palindrome '(a b c)))))))
