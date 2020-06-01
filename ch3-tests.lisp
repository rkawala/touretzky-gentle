(in-package #:touretzky)

(deftest sec3.8
    (testing "some symbols are also values"
             (ok (eq 't t))
      (ok (eq 'nil nil))))

(deftest ex3.15
  (testing "quotes and variables"
    (flet ((scrabble (word) (list word 'is 'a 'word)))
      (ok (equal '(aardvark is a word) (scrabble 'aardvark)))
      (ok (equal '(word is a word) (scrabble 'word))))))

(deftest ex3.16
  (testing "parameters can be weird"
    (flet ((stooge (larry moe curly) (list larry (list 'moe curly) curly 'larry)))
      (ok (equal '(moe (moe larry) larry larry) (stooge 'moe 'curly 'larry))))))

(deftest sec3.17
  (testing "multiple apostrophes converted to quote, in a way I don't understand"
    (ok (eq 'foo 'foo))
    (ok (equal '(quote foo) ''foo))
    (ok (equal (quote (quote foo)) ''foo))
    (ok (equal '(quote (quote foo)) '''foo))
    (ok (eq 'foo (eval (list 'quote 'foo))))))

(deftest sec3.21
  (testing "hash-apostrophe gets you the function"
    (ok (eq (symbol-function 'cons) #'cons)))
  (testing "apply is kind of like eval with different syntax"
    (ok (= 5 (apply #'+ '(2 3))))))
