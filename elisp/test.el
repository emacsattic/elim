(setq eproc (elim-start))
(setq uname (user-login-name))
(elim-add-account eproc (concat uname "-elim@irc.freenode.net") "prpl-irc" nil)
(elim-connect     eproc (concat uname "-elim@irc.freenode.net"))
(elim-disconnect  eproc (concat uname "-elim@irc.freenode.net"))
(elim-join-chat   eproc (concat uname "-elim@irc.freenode.net") "#emacs" 
                  '("channel" "#emacs"))

(elim-join-chat   eproc (concat uname "-elim@irc.freenode.net") "#elim" 
                  '("channel" "#elim"))

;; you'll need to grab conbversation UIDs from *Messages*
;; accounts can be identified by either uid or name+protocol
(elim-process-send eproc
  (elim-daemon-call 'message nil
    `(alist nil
       (string ((name . "account-name"))
               ,(concat uname "-elim@irc.freenode.net"))
       (string ((name . "im-protocol" )) "prpl-irc" )
       (int    ((name . "conv-uid"    )) "134985920")
       (string ((name . "text"        )) "wootles"  ))))

(elim-process-send eproc
  (elim-daemon-call 'message nil
    `(alist nil
       (string ((name . "account-name")) 
               ,(concat uname "-elim@irc.freenode.net"))
       (string ((name . "conv-name"  )) "fledermaus")
       (string ((name . "text"       )) 
               ,(read-string "IM> " "" nil "elim-test-string" t)))))


(elim-process-send eproc
  (elim-daemon-call 'list-accounts nil nil))


(elim-process-send eproc
  (elim-daemon-call 'list-protocols nil nil))


(elim-fetch-process-data eproc 'accounts)
(elim-fetch-process-data eproc 'protocols)

(process-plist eproc)


(progn
  (mapcar 
   (lambda (B) (when B (kill-buffer B)))
   (list (get-buffer "*elim*")
         (get-buffer "*elim-proto*")))
  (setq eproc nil))

(setq eproc (elim-start))


(elim-list-accounts-response 
      eproc nil nil nil 
      '(("status" . 0) 
        ("value" 
         ("134892792" 
          ("account-name" . "vivek-elim@irc.freenode.net") 
          ("im-protocol"  . "prpl-irc") 
          ("connected") 
          ("account-uid" . 134892792)))))