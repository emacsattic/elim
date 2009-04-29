(setq eproc (elim-start))
(setq uname (user-login-name))
(elim-add-account eproc (concat uname "-elim@irc.freenode.net") "prpl-irc" nil)
(elim-connect     eproc (concat uname "-elim@irc.freenode.net"))
(elim-disconnect  eproc (concat uname "-elim@irc.freenode.net"))
(elim-join-chat   eproc (concat uname "-elim@irc.freenode.net") "#emacs" 
                  '("channel" "#emacs"))

;; you'll need to grab conbversation UIDs from *Messages*
;; accounts can be identified by either uid or name+protocol
(elim-process-send eproc
  (elim-daemon-call 'message nil
    `(alist nil
       (string ((name . "account-name"))
               ,(concat uname "-elim@irc.freenode.net"))
       (string ((name . "im-protocol" )) "prpl-irc" )
       (int    ((name . "conv-uid"    )) "135018696")
       (string ((name . "text"        )) "bye"      ))))

(elim-process-send eproc
  (elim-daemon-call 'message nil
    `(alist nil
       (string ((name . "account-name")) 
               ,(concat uname "-elim@irc.freenode.net"))
       (string ((name . "conv-name"  )) "fledermaus")
       (string ((name . "text"       )) 
               ,(read-string "IM> " "" nil "elim-test-string" t)))))

(elim-fetch-process-data eproc 'accounts)
(elim-fetch-process-data eproc 'protocols)

(process-plist eproc)

(progn
  (mapcar 
   (lambda (B) (when B (kill-buffer B)))
   (list (get-buffer "*elim*")
         (get-buffer "*elim-proto*")))
  (setq eproc nil))