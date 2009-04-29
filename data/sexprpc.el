(function-call nil
   (some-function ((id . "123456789")))
   (alist nil
          (item ((name  . "text"    )) (string nil "beans" ))
          (item ((name  . "integer" )) (int    nil "42"    ))
          (item ((name  . "number"  )) (float  nil "3.1415"))
          (item ((name  . "true"    )) (bool   nil "1"     ))
          (item ((name  . "false"   )) (bool   nil "0"     ))
          (item ((name  . "hash"    )) 
                (alist nil  
                       (item ((name . "abc")) (string nil "123"))
                       (item ((name . "def")) (int    nil "345")) ))
          (item ((name  . "sequence")) 
                (list  nil 
                       (string nil "zero")
                       (int    nil "1"   )
                       (float  nil "2.0" ))) ) )

(function-response nil
  (other-function ((id . "123456790")))
  (alist nil
    (item ((name . "status"))
          (int nil "22"))
    (item ((name . "message"))
          (string nil "unknown function"))))

(function-response nil
  (other-function ((id . "123456790")))
  (alist nil
    (item ((name . "status"))
          (int nil "22"))
    (item ((name . "message"))
          (string nil "unknown function"))))

(function-call nil
  (init ((id . "1234567891"))) 
  (alist nil
    (item ((name . "dot-dir")) 
          (string "/home/vivek/.emacs.d/elim"))))

(function-call nil 
  (list-protocols ((id . "1234567892")))
  (alist nil))
