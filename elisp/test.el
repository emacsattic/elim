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
    '(alist nil
       (string ((name . "account-name"))(concat uname "-elim@irc.freenode.net"))
       (string ((name . "im-protocol" )) "prpl-irc")
       (int    ((name . "conv-uid"    )) "134973608")
       (string ((name . "text"        )) (shell-command-to-string "fortune")))))

(elim-process-send eproc
  (elim-daemon-call 'message nil
    '(alist nil
       (int    ((name . "account-uid")) "135020992")
       (int    ((name . "conv-uid"   )) "134995528")
       (string ((name . "text"       )) "hmm..."   ))))

(elim-fetch-process-data eproc 'accounts)
(elim-fetch-process-data eproc 'protocols)

(process-plist eproc)

(progn
  (mapcar 
   (lambda (B) (when B (kill-buffer B)))
   (list (get-buffer "*elim*")
         (get-buffer "*elim-proto*")))
  (setq eproc nil))