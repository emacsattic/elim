(add-to-list 'load-path "~/src/elisp/lui/")
(require 'lui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer tracking data structures
(defvar garak-conversation-buffers      nil)
(defvar garak-dead-conversation-buffers nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables local to a garak conversation buffer
(defvar garak-elim-process nil)
(defvar garak-account-name nil)
(defvar garak-im-protocol  nil)
(defvar garak-account-uid  nil)
(defvar garak-conv-uid     nil)
(defvar garak-conv-name    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; callback lookup:
(defvar garak-callbacks 
  '( ;;(elim-account-notify-added)
     (elim-account-status-changed . garak-account-status      )
     (elim-account-request-add    . garak-stranger-added-you  )
     (elim-account-request-auth   . garak-auth-requested      )
     ;; blist (buddy list) ops
     ;;(elim-blist-create-node                              )
     (elim-blist-update-node      . garak-update-buddy        )
     (elim-blist-remove-node      . garak-delete-buddy        )
     ;; connection ops
     (elim-connection-state       . garak-connection-state    )
     (elim-connection-progress    . garak-connection-progress )
     (elim-disconnect-reason      . garak-disconnected        )
     ;; network status
     (elim-network-up             . garak-network-up          )
     (elim-network-down           . garak-network-down        )
     ;; conversation
     (elim-conv-create            . garak-new-conversation    )
     (elim-conv-destroy           . garak-end-conversation    )
     ;; messages:
     (elim-conv-write-chat        . garak-chat-message        )
     (elim-conv-write-im          . garak-user-message        )
     (elim-conv-write-sys         . garak-misc-message)       )
  "Alist of elim callbacks and their corresponding handlers")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer management infrastructure
(defun garak-init-local-storage ()
  (mapc 'make-local-variable
        '(garak-elim-process
          garak-account-name
          garak-im-protocol
          garak-account-uid 
          garak-conv-uid    
          garak-conv-name   )))

(defun garak-conversation-buffer (args &optional do-not-create)
  (let ((uid (cdr (assoc "conv-uid" args))) buffer)
    (when uid (setq buffer (cdr (assoc uid garak-conversation-buffers))))
    (when (and (not buffer) (not do-not-create))
      (progn
        (setq buffer (generate-new-buffer (cdr (assoc "conv-name" args)))
              garak-conversation-buffers 
              (cons (cons uid buffer) garak-conversation-buffers)))) 
    buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; message callbacks
(defun garak-chat-message (process call call-id status args)
  (let ( (buffer (garak-conversation-buffer args t)) 
         (flags  (cdr (assoc "flags" args))) 
         text who)
    (when (not buffer)
      (setq buffer (garak-new-conversation process call call-id status args)))
    (with-current-buffer buffer
      (setq text (cdr (assoc "text" args))
            who  (or (cdr (assoc "alias" args)) 
                     (cdr (assoc "who"   args))))
      (if (memq :system flags)
          (lui-insert (format "* %s *" text))
        (lui-insert (format "<%s> %s" (or who garak-account-name) text)) )) ))

(defalias 'garak-user-message 'garak-chat-message)
(defalias 'garak-misc-message 'garak-chat-message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conv callbacks
(defun garak-new-conversation (process call call-id status args)
  (let ((buffer (garak-conversation-buffer args t)))
    (when (not buffer)
      (setq buffer (garak-conversation-buffer args))
      (with-current-buffer buffer
        (garak-mode)
        (garak-init-local-storage)
        (setq garak-elim-process process
              garak-account-name (cdr (assoc "account-name" args))
              garak-account-uid  (cdr (assoc "account-uid"  args))
              garak-conv-name    (cdr (assoc "conv-name"    args))
              garak-conv-uid     (cdr (assoc "conv-uid"     args))
              garak-im-protocol  (cdr (assoc "im-protocol"  args))) 
        (lui-insert (format "*%s / %s*" garak-account-name garak-conv-name)) ))
      buffer))

(defun garak-end-conversation (process call call-id status args)
  (let ((buffer (garak-conversation-buffer args t)))
    (when buffer
      (with-current-buffer buffer
        (setq garak-dead-conversation-buffers
              (cons buffer garak-dead-conversation-buffers)
              garak-conversation-buffers
              (rassq-delete-all buffer garak-conversation-buffers))
        (lui-insert "*conversation ended*") )) ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commands
(defun garak-read-username (proc proto)
  (read-string "user name: " nil nil user-login-name t))

(defun garak-read-password (proc proto)
  (read-passwd "password: " t))

(defun garak-cmd-add-account (args)
  (let (items user proto pass options elim errval)
    (setq items (split-string args)
          user  (car  items)
          proto (cadr items)
          items (cddr items))
    (setq elim garak-elim-process)
    (when (= (length proto) 0) (setq proto (garak-read-protocol elim)))
    (when (= (length user ) 0) (setq user  (garak-read-username elim proto)))
    (when (and (car items) (not (string-match "=" (car items))))
      (setq pass (car items) items (cdr items)))
    (when (= (length pass ) 0) (setq pass  (garak-read-password elim proto)))
    ;; options not supported yet:
    ;;(mapcar
    ;; (lambda (O) (setq options (nconc options (split-string "=" O)))) items)
    (condition-case errval
        (progn
          (elim-add-account elim user proto pass options)
          (format "/add-account %s" args))
        (error (format "%S" errval))) ))

(defun garak-cmd-add-buddy (args)
  (let (items account proto buddy errval)
    (setq items (split-string args))
    (if (equal (length items) 2)
        (setq account (car  items)
              buddy   (cadr items))
      (setq account garak-account-uid
            buddy   (car items)))
    (condition-case errval
        (progn
          (elim-add-buddy garak-elim-process account buddy)
          (format "/add-buddy %s" args))
        (error "Could not add buddy: %S" errval)) ))

(defun garak-read-join-parameters (spec items)
  (let (options secret value key required)
    (while spec 
      (setq value    (car items)
            key      (caar spec)
            secret   (cdr (assoc "secret"   (cdar spec)))
            required (cdr (assoc "required" (cdar spec))))
      (when (or (and (or (not value) (equal value "-")) required)
                (and (equal value "-") secret))
        (setq value 
              (if secret 
                  (read-passwd (concat key ": "))
                (read-string (concat key ": ") nil nil nil t))))
      (when value (setq options (cons value (cons key options))))
      (setq spec  (cdr  spec)
            items (cdr items)))
    (nreverse options)))

(defun garak-cmd-join (args)
  (let (items account account-data proto spec options rval)
    (setq rval         (format "/join %s (failed)" args)
          items        (split-string args)
          account-data (elim-account-data garak-elim-process (car items))
          account      (car account-data))
    (if account
        (setq items (cdr items))
      (message "account defaulting to %S" garak-account-uid)
      (setq account      garak-account-uid
            account-data (elim-account-data garak-elim-process account)))
    (if account-data 
        (progn 
          (setq proto (cdr (assq :proto (cdr account-data)))
                spec  (elim-chat-parameters garak-elim-process proto))
          (message "arg-spec: %S" spec)
          (if spec 
              (if (setq options (garak-read-join-parameters spec items))
                  (progn 
                    (message "e-j-c: process %S %S %S" account "" options)
                    (elim-join-chat garak-elim-process account "" options) 
                    (format "/join %s" args))
                (format "/join %s: args not valid" args))
            (format "/join %s: protocol plugin does not support command")))
      (format "/join %s: no account found" args)) ))

(defun garak-cmd-connect (args)
  (let ((account-data (elim-account-data garak-elim-process args)))
    (if (not account-data)
        (format "/connect %s: no account found" args)
      (elim-connect garak-elim-process args)
      (format "/connect %s" (cdr (assq :name (cdr account-data)))) )))

(defun garak-cmd-disconnect (args)
  (let (account-data)
    (setq account-data 
          (or (elim-account-data garak-elim-process args             ) 
              (elim-account-data garak-elim-process garak-account-uid)))
    (if (not account-data)
        (format "/disconnect %s: no account found" args)
      (elim-disconnect garak-elim-process (car account-data))
      (format "/disconnect %s" (cdr (assq :name (cdr account-data)))) )))

(defun garak-cmd-not-implemented (args)
  "Command not implemented")

(defun garak-command-handler (cmd args)
  (let ((handler nil))
    (setq handler
          (cond
           ((string-match "^add.account"         cmd) 'garak-cmd-add-account)
           ((string-match "^add.buddy"           cmd) 'garak-cmd-add-buddy  )
           ((string-match "^connect"             cmd) 'garak-cmd-connect    )
           ((string-match "^login"               cmd) 'garak-cmd-connect    )
           ((string-match "^disconnect"          cmd) 'garak-cmd-disconnect )
           ((string-match "^logout"              cmd) 'garak-cmd-disconnect )
           ((string-match "^logoff"              cmd) 'garak-cmd-disconnect )
           ((string-match "^\\(?:help\\|\\?\\)"  cmd) 'garak-cmd-help       )
           ((string-match "^join\\(?:.\\S-+\\)?" cmd) 'garak-cmd-join       )))
    (elim-debug "handler: %S" handler)
    (if (fboundp handler)
        (progn 
          (funcall handler (or args "")))
      (format "unrecognised command: /%s %s" cmd (or args "")))))

(defun garak-input-handler (input)
  (let ((output input) (command nil) (args nil))
    (if (string-match "^/\\(\\S-+\\)\\s-*\\(.*\\)" input)
        (setq command (match-string 1 input)
              args    (match-string 2 input)
              output  (garak-command-handler command args)) 
      (elim-message garak-elim-process garak-account-uid garak-conv-uid output)
      (setq output nil))
  (when output (lui-insert output)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode garak-mode lui-mode "Garak"
  "An IM mode based on elim"
  (setq lui-input-function 'garak-input-handler)
  (lui-set-prompt (concat (propertize "garak>" 'face 'mode-line) " ")))

(defun garak ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "*garak*"))
  (garak-mode)
  (setq lui-input-function 'garak-input-handler)
  (lui-insert "starting elim" t)
  (set (make-local-variable 'garak-elim-process) 
       (elim-start "garak" nil garak-callbacks))
  (end-of-buffer))

