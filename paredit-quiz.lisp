;;; Q1 (add a sexp in the middle of an existing sexp)

(a b d e)

-->

(a b (c) d e)

;;; Q2-a (wrap some part of an existing sexp in parens, creating a new inner sexp)

(a b c)

-->

(a (b) c)

;;; Q2-b (use mark)

(a b c)

-->

(a (b c))

;;; Q2-c (use count)

(a b (b1 b2 (b3 b4)) c)

-->

(a (b (b1 b2 (b3 b4))) c)

;;; Q3 (delete whitespace at the end of an existing sexp)

(a   )

-->

(a)

;;; Q4 (split sexp into two lines)

(a (b) c)


-->

(a (b)
   c)

;;; Q5-a (delete from point to the end of the sexp)

(a b c)

-->

(a)

;;; Q5-b (delete from point to the end of the sexp, including nested sexps)

(a (b (b1 b2) c) d)

-->

(a)

;;; Q6-a (split sexp into two)

(a b)

-->

(a) (b)

;;; Q6-b (more complicated example of splitting sexp into two)

(a b c d)

-->

(a b c) (d)

;;; Q7 (join two sexps)

(a b) (c d)

-->

(a b c d)

;;; Q8 (split a string in two)

("a b c d")

-->

("a b" "c d")

;;; Q9 (join two strings)

("a b" "c d")

-->

("a bc d")

;;; Q10 (reformat; doesn't work)

(a (b) ((c d e) (f g h) (i j k)))

-->

(defun a    (b) ((c
                  d e) (f g h) (i j k)))


;;; Q11 (remove outer set of parens around a sexp)

(((((a b)))))

-->

(a b)
