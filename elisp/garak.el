(require 'lui  nil t)
(require 'elim nil t)

(let ((load-path (cons (file-name-directory 
                        (or load-file-name buffer-file-name)) load-path)))
  (when (not (featurep 'lui )) (require 'lui ))
  (when (not (featurep 'elim)) (require 'elim)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; faces
(defface garak-nick-face '((default (:foreground "salmon"))) 
  "Default face for IM user names")

(defface garak-own-nick-face '((default (:foreground "aquamarine"))) 
  "Default face your IM user name(s)")

(defface garak-system-message-face '((default (:foreground "grey"))) 
  "Default face for system (non user generated) messages")

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
     ;;(elim-blist-create-node                                )
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
     (elim-chat-add-users         . garak-chat-add-users      )
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
    (when (and (bufferp buffer) (not (buffer-live-p buffer)))
      (rassq-delete-all buffer garak-conversation-buffers)
      (setq buffer nil))
    (when (and (not buffer) (not do-not-create))
      (progn
        (setq buffer (generate-new-buffer (cdr (assoc "conv-name" args)))
              garak-conversation-buffers 
              (cons (cons uid buffer) garak-conversation-buffers)))) 
    buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buddy management:
(defvar garak-inserted-buddies nil)
(defun garak-insert-buddy-list (process)
  (let ((blist (elim-buddy-list process)) (top-level-uids nil))
    (mapc 
     (lambda (N)
       (when (eq (cdr (assoc "bnode-type" (cdr N))) :group-node)
         (garak-insert-buddy-item process N 0)
         ;;(message "insert top level buddy: %S" N) 
         )) blist) ))

(defun garak-insert-buddy-spacer (level)
  (when (and level (< 0 level))
    (insert (make-string (1- level) ?\ ) "+-" )))

(defun garak-insert-buddy-item (proc buddy &optional level)
  (let ((uid (car buddy)) (data (cdr buddy)) name children next)
    (setq level (or level 0)
          name  (cdr (assoc "bnode-name"  data))
          next  (cdr (assoc "bnode-prev"  data)));;NOTE. -next empty: use -prev
    (garak-insert-buddy-spacer level)
    (setq children (elim-buddy-children proc uid))
    ;;(insert (format "%S\n" data))
    (insert (propertize (format "[%s]\n" (or name "+")) :garak-bnode-uid uid))
    (mapc 
     (lambda (C) 
       (garak-insert-buddy-item proc C (1+ level))) children) ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; message callbacks
(defun garak-chat-message (process call call-id status args)
  (let ( (buffer (garak-conversation-buffer args t)) 
         (flags  (cdr (assoc "flags" args))) 
         text who nick-face)
    (when (not buffer)
      (setq buffer (garak-new-conversation process call call-id status args)))
    (with-current-buffer buffer
      (setq text (cdr (assoc "text" args))
            who  (or (cdr (assoc "alias" args)) 
                     (cdr (assoc "who"   args))))
      (if (memq :system flags)
          (lui-insert 
           (elim-add-face (format "* %s *" (elim-interpret-markup text))
                          'garak-system-message-face))
        (setq nick-face (if (memq :send flags)
                            'garak-own-nick-face
                          'garak-nick-face))
        (lui-insert 
         (format "<%s> %s" 
                 (elim-add-face (or who garak-account-name) nick-face) 
                 text)) )) ))

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

(defun garak-chat-add-users (process call call-id status args) 
  (let ((buffer (garak-conversation-buffer args t)) 
        people name new message cname)
    (when (not buffer) 
      (setq buffer (garak-new-conversation process call call-id status args)))
    (when buffer
      (with-current-buffer buffer
        (setq new     (elim-avalue "new-arrivals" args)
              cname   (elim-avalue "conv-name"    args)
              people  (elim-avalue "participants" args)
              people  (mapcar (lambda (P) (elim-avalue "name" P)) people))
        (message "CHAT ADD: %S" new)
        (if (not new)
            (setq message (mapconcat 'identity people " ")
                  message (concat "* Users in " cname ": " message)
                  message (elim-add-face message 'garak-system-message-face))
          (setq message
                (mapcar 
                 (lambda (N) 
                   (elim-add-face (format "* %s has entered %s" N cname) 
                                  'garak-system-message-face)) people)))
        (message "CHAT ADD: %S" message)
        (if (listp message)
            (mapc 'lui-insert message)
          (lui-insert message)) )) ))

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

(defun garak-cmd-leave (args)
  (if (not garak-conv-uid)
      "/part: not a conversation buffer"
    (elim-leave-conversation garak-elim-process garak-conv-uid)
    "/part"))

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

(defvar garak-command-handlers
  '((add-account . garak-cmd-add-account)
    (add-buddy   . garak-cmd-add-buddy  )
    (connect     . garak-cmd-connect    )
    (disconnect  . garak-cmd-disconnect )
    (help        . garak-cmd-help       )
    (join        . garak-cmd-join       )
    (leave       . garak-cmd-leave      )
    (quit        . garak-cmd-quit       )))

(defun garak-command-match (cmd)
  (cond
   ((string-match "\\(?:^\\|/\\)add.account\\>"         cmd) 'add-account)
   ((string-match "\\(?:^\\|/\\)add.buddy\\>"           cmd) 'add-buddy  )
   ((string-match "\\(?:^\\|/\\)connect\\>"             cmd) 'connect    )
   ((string-match "\\(?:^\\|/\\)login\\>"               cmd) 'connect    )
   ((string-match "\\(?:^\\|/\\)disconnect\\>"          cmd) 'disconnect )
   ((string-match "\\(?:^\\|/\\)logout\\>"              cmd) 'disconnect )
   ((string-match "\\(?:^\\|/\\)logoff\\>"              cmd) 'disconnect )
   ((string-match "\\(?:^\\|/\\)part\\>"                cmd) 'leave      )
   ((string-match "\\(?:^\\|/\\)leave\\>"               cmd) 'leave      )
   ((string-match "\\(?:^\\|/\\)\\(?:help\\>\\|\\?\\)"  cmd) 'help       )
   ((string-match "\\(?:^\\|/\\)join\\(?:.\\S-+\\)?\\>" cmd) 'join       )
   ((string-match "\\(?:^\\|/\\)quit\\>"                cmd) 'quit       )))

(defun garak-command-handler (cmd args)
  (let (command handler)
    (setq command (garak-command-match cmd)
          handler (cdr (assq command garak-command-handlers)))
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
      (set-text-properties 0 (length output) nil output)
      (elim-message garak-elim-process garak-account-uid garak-conv-uid output)
      (setq output nil))
  (when output (lui-insert output)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion:
(defvar garak-commands 
  '("/add-account" "/add-buddy" "/connect" "/login"
    "/disconnect"  "/logout"    "/logoff"  "/quit" ))

(defvar garak-command-completers 
  '((add-account . garak-comp-add-account)
    (add-buddy   . garak-comp-add-buddy  )
    (connect     . garak-comp-connect    )
    (disconnect  . garak-comp-disconnect )
    (help        . garak-comp-help       )
    (join        . garak-comp-join       )))

(defun garak-comp-add-account (prefix &optional protocol)
  (let (proto args available)
    (setq args      (split-string prefix split-string-default-separators nil)
          available (mapcar 'car (elim-protocol-alist garak-elim-process))
          proto     (or (nth 2 args) ""))
    ;;(message "rval: %S" (all-completions proto available))
    (when (and (= (length args) 3) (not (member proto available)))
      (all-completions proto available)) ))

(defun garak-comp-add-buddy   (prefix &optional protocol)
  (let (acct args available)
    (setq args      (split-string prefix split-string-default-separators nil)
          available (mapcar (lambda (A) (cdr (assq :name (cdr A))))
                            (elim-account-alist garak-elim-process))
          acct      (or (nth 1 args) ""))
    (when (and (= (length args) 2) (not (member acct available)))
      (all-completions acct available)) ))

(defun garak-comp-connect     (prefix &optional protocol)
  (garak-comp-add-buddy prefix))
(defun garak-comp-disconnect  (prefix &optional protocol)
  (garak-comp-add-buddy prefix))

(defun garak-comp-help (prefix &optional protocol)
  (let (cmd args)
    (setq args (split-string prefix split-string-default-separators nil)
          cmd  (or (nth 1 args) ""))
    (when (and (= (length args) 2) (not (member cmd garak-commands)))
      (all-completions cmd garak-commands)) ))

(defun garak-comp-join (prefix &optional protocol))

(defun garak-complete-commands (&optional prefix protocol) 
  (if (zerop (length prefix))
      garak-commands
    (cons (try-completion  prefix garak-commands) 
          (all-completions prefix garak-commands)) ))

(defun garak-complete (&optional at-start prefix)
  (if at-start
      (garak-complete-commands nil garak-im-protocol)
    (when (not prefix)
      (setq prefix (buffer-substring lui-input-marker (point))))
    (let ((completer (garak-command-match prefix)))
      (when (setq completer (cdr (assq completer garak-command-completers)))
        (funcall completer prefix garak-im-protocol)) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode garak-mode lui-mode "Garak"
  "An IM mode based on elim"
  (setq lui-input-function 'garak-input-handler)
  (set (make-local-variable 'lui-possible-completions-function) 
       'garak-complete)
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

(provide 'garak)