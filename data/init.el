(function-call nil
  (init ((id . "1234567891"))) 
  (alist nil
         (string ((name . "dot-dir")) "/home/vivek/.emacs.d/elim")) )

(function-call nil 
  (list-protocols ((id . "1234567892"))))

'(function-call nil
  (bogus-test-function ((id . "42"))) 

 (let ((sexp
        '(alist nil
                (string ((name . "abc")) "def")
                (int    ((name . "def")) "123")
                (list   ((name . "ghi"))
                        (string nil "first" )
                        (string nil "second")
                        (string nil "third" ))) ))
   (setq sexp (nconc (list (car sexp) '((name . "foo"))) (cddr sexp)) ) )