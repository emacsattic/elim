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
       (int    ((name . "conv-uid"   )) "135065544")
       (string ((name . "text"       )) 
               ,(read-string "IM> " "" nil "elim-test-string" t)))) )

(elim-process-send eproc
  (elim-daemon-call 'enumerations nil `(alist nil)))

(elim-process-send eproc
  (elim-daemon-call 'enumerations nil 
   `(alist nil (string ((name . "enum-type")) ":connection-error"))) )

(elim-process-send eproc
  (elim-daemon-call 'chat-params nil 
   '(alist nil (string ((name . "im-protocol")) "prpl-jabber"))))

(elim-process-send eproc
  (elim-daemon-call 'list-accounts nil nil))

(elim-process-send eproc
  (elim-daemon-call 'list-protocols nil nil))


(elim-fetch-process-data eproc :accounts)
(elim-fetch-process-data eproc :protocols)
(elim-fetch-process-data eproc :initialised)

(process-plist eproc)


(progn
  (mapcar 
   (lambda (B) (when B (kill-buffer B)))
   (list (get-buffer "*elim*")
         (get-buffer "*elim-proto*")))
  (setq eproc nil))

;; !!
(setq eproc (elim-start ))


(elim-parse-proto-args 
 '(alist nil
         (int   ((name . "status")) "0")
         (alist ((name . "value" )) 
                (string ((name . "a-string")) "sashdksaj")
                (bool   ((name . "true"    )) "1"        )
                (bool   ((name . "false"   )) "0"        )
                (float  ((name . "π"       )) "3.1415"   )
                (int    ((name . "mfeat"             )
                         (type . ":message-flags"    )) "16438")
                (int    ((name . "ctype"             )
                         (type . ":conversation-type")) "3"))))



(setq eproc (get-process "*elim*"))
(delete-process eproc)
(elim-load-enum-values (get-process "*elim*"))