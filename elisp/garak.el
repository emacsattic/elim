(require 'lui  nil t)
(require 'elim nil t)
(require 'widget)
(require 'wid-edit)
(require 'tree-widget)
(defvar garak-icon-directory nil)

(let ((load-path (cons (file-name-directory 
                        (or load-file-name buffer-file-name)) load-path)))
  (setq garak-icon-directory 
        (concat (file-name-directory 
                 (or load-file-name buffer-file-name)) "../icons/"))
  (when (not (featurep 'lui )) (require 'lui ))
  (when (not (featurep 'elim)) (require 'elim)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; faces
(defface garak-nick-face '((default (:foreground "salmon"))) 
  "Default face for IM user names")

(defface garak-marker-face '((default (:foreground "palegreen"))) 
  "Default face for markers etc inserted into garak buffers")

(defface garak-warning-face '((default (:foreground "red"))) 
  "Default face for IM warnings")

(defface garak-own-nick-face '((default (:foreground "aquamarine"))) 
  "Default face your IM user name(s)")

(defface garak-system-message-face '((default (:foreground "grey"))) 
  "Default face for system (non user generated) messages")

(defface garak-emote-face '((default (:foreground "palevioletred")))
  "Face for /me style emotes.")

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
;; icons
(defvar garak-icons 
  (delq nil
        (mapcar
         (lambda (attr)
           (when (or (stringp (cadr attr))
                     (null    (cadr attr)))
             (let ((label (concat ":" (file-name-sans-extension (car attr))))
                   (path  (concat garak-icon-directory (car attr))))
               (cons label 
                     (create-image path nil nil
                                   :pointer 'hand
                                   :ascent  'center
                                   :mask '(heuristic t)))) ))
         (directory-files-and-attributes garak-icon-directory)) ))

(defvar garak-icon-tags '((":available"   . "[!]" )
                          (":away"        . "[_]" )
                          (":busy"        . "(-)" )
                          (":group"       . "{ }")
                          (":unavailable" . "(-)" )
                          (":chat"        . " @ " )
                          (":offline"     . "[ ]" )))

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
     (command                     . garak-command-response    )
     ;; messages:
     (elim-conv-write-chat        . garak-chat-message        )
     (elim-conv-write-im          . garak-user-message        )
     (elim-conv-write-sys         . garak-misc-message        ) )
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
;; message callbacks
(defun garak-abbreviate-nick (nick &optional protocol)
  (if (string-match "^\\(.*?\\)@" nick) (match-string 1 nick) nick))

(defun garak-command-response (process call call-id status args)
  (let ((buffer (garak-conversation-buffer args t)) cmd cstatus err)
    (message "garak-command-response %S %S %S" call status args)
    (if buffer
      (with-current-buffer buffer
        (if (not (zerop status))
            (lui-insert (elim-add-face args 'garak-warning-face))
          (setq cmd     (elim-avalue "command-line"   args)
                cstatus (elim-avalue "command-status" args)
                err     (or (elim-avalue "command-error"  args)
                            (elim-avalue "command-status" args)))
          (if (not (eq :ok cstatus))
              (lui-insert (elim-add-face (format "/%s: %s" cmd err) 
                                         'garak-warning-face))
            (lui-insert (elim-add-face (format "/%s" cmd) 
                                       'garak-system-message-face))) ))
      (message "%S response %s %S: no target buffer" call call-id args)) ))

(defun garak-time-since (time-t)
  (let ((delay    (- (float-time) time-t))
        (dlabel   nil)
        (ilabel   "")
        (handled   0)
        (absolute (format-time-string "%Y-%m-%d %T %Z" 
                                      (seconds-to-time time-t)))) 
    (when (< 86400 delay)
      (setq handled (round delay 86400.0)
            delay   (mod delay 86400)
            dlabel  (format "%d day%s" handled    (if (< 1 handled) "s" ""))))
    (message "dlabel: %s" dlabel)
    (when (< 3600 delay)
      (setq handled (round delay 3600.0)
            delay   (mod delay 3600)
            ilabel  (format "%d hour%s" handled   (if (< 1 handled) "s" ""))
            dlabel  (concat dlabel (when dlabel " ") ilabel)))
    (message "dlabel: %s" dlabel)
    (when (< 60 delay)
      (setq handled (round delay 60.0)
            delay   (mod delay 60)
            ilabel  (format "%d minute%s" handled (if (< 1 handled) "s" ""))
            dlabel  (concat dlabel (when dlabel " ") ilabel)))
    (message "dlabel: %s" dlabel)
    (when (< 0 delay)
      (setq ilabel (format "%d second%s" delay    (if (< 1 delay  ) "s" ""))
            dlabel (concat dlabel (when dlabel " ") ilabel)))
    (message "dlabel: %s" dlabel)
    (format "%s (%s ago)" absolute dlabel)))

(defun garak-chat-message (process call call-id status args)
  (let ( (buffer  (garak-conversation-buffer args t)) 
         (flags   (cdr (assoc "flags" args)))
         (mformat "<%s> %s")
         text who nick-face title when stamp)
    (when (not buffer)
      (setq buffer (garak-new-conversation process call call-id status args)))
    (with-current-buffer buffer
      (setq text (cdr (assoc "text" args))
            who  (or (cdr (assoc "alias" args)) 
                     (cdr (assoc "who"   args))
                     (garak-abbreviate-nick garak-account-name) ))
      (if (memq :system  flags)
          (lui-insert 
           (elim-add-face (format "* %s *" (elim-interpret-markup text))
                          'garak-system-message-face))
        (if (memq :send flags)
            (setq nick-face 'garak-own-nick-face)
          (setq nick-face 'garak-nick-face)
          (when (eq (elim-avalue "conv-type" args) :im)
            (setq title (elim-avalue "conv-title" args))
            (when (and title (< (length title) (length who)))
              (setq who title))))
        (when (string-match "^/me " text)
          (setq text   (replace-regexp-in-string "^/me " "" text)
                mformat "* %s %s")
          (elim-add-face text 'garak-emote-face))
        ;; ok we're all set up. if this is a delayed message, insert 
        ;; the delay marker
        (when (memq :delayed flags)
          (setq when  (elim-avalue "time" args)
                stamp (garak-time-since when))
          (lui-insert 
           (elim-add-face (format "[%s]" stamp) 'garak-marker-face)))
        (lui-insert (format mformat (elim-add-face who nick-face) text)) )) ))

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
        ;;(message "CHAT ADD: %S" new)
        (when (not new)
          (setq message (mapconcat 'identity people " ")
                message (concat "* Users in " cname ": " message)
                message (elim-add-face message 'garak-system-message-face)))
          ;;(setq message
          ;;      (mapcar 
          ;;       (lambda (N) 
          ;;         (elim-add-face (format "* %s has entered %s" N cname) 
          ;;                        'garak-system-message-face)) people)))
        ;;(message "CHAT ADD: %S" message)
        (if (listp message)
            (mapc 'lui-insert message)
          (when message (lui-insert message))) )) ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI functions (forms etc)
(defun garak-account-param-widget (field)
  (let ((id   (cons "fields" (car field))) 
        (data (cdr field)))
    (elim-request-field (cons id data)) ))

(defun garak-ui-cancel-cb (&optional parent child event &rest stuff)
  (when elim-form-ui-args (kill-buffer nil)))

(defun garak-ui-account-options-ok-cb (&optional parent child event &rest stuff)
  (when elim-form-ui-args 
    (let (proc account arglist account-arg)
      (setq proc        (cadr (memq :process elim-form-ui-args))
            account     (cadr (memq :account elim-form-ui-args))
            account-arg (elim-atom-to-item "account-uid" account)
            arglist     (elim-form-proto-values elim-form-ui-data)
            arglist     (cons account-arg arglist)
            arglist     (nconc (list 'alist nil) arglist))
      (elim-process-send proc 
                         (elim-daemon-call 'set-account-options nil arglist)))
    (kill-buffer (current-buffer)) ))

(defun garak-account-options-ui-cb (proc name id attr args)
  (let ((status (elim-avalue "status" args)) 
        (value  (elim-avalue "value"  args)))
    (when (and (numberp status) (zerop status))
      (let ((password      (elim-avalue "password"      value))
            (account-alias (elim-avalue "account-alias" value))
            (save-password (elim-avalue "save-password" value))
            (fields        (elim-avalue "fields"        value))
            (protocol      (elim-avalue "protocol"      value))
            (account-name  (elim-avalue "account-name"  value))
            (account-uid   (elim-avalue "account-uid"   value))
            title buf-name ui-buffer)
        (setq title     (format "%s Options: %s" protocol account-name)
              buf-name  (format "* %s *" title)
              ui-buffer (generate-new-buffer buf-name))
        (with-current-buffer ui-buffer
          (elim-init-ui-buffer)
          (setq elim-form-ui-args (list :process proc :account account-uid))
          (widget-insert title "\n")
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; fixed items
          (widget-insert "\n")
          (elim-request-field-string "password"      
                                     (list (cons "label" "Password     ")
                                           (cons "masked" t             )
                                           (cons "value"  password      )))
          (widget-insert "\n\n")
          (elim-request-field-string "account-alias" 
                                     (list (cons "label" "Local Alias  ")
                                           (cons "value"  account-alias )))
          (widget-insert "\n\n")
          (elim-request-field-toggle "save-password"
                                     (list (cons "label" "Save Password")
                                           (cons "value"  save-password )))
          (widget-insert "\n")
          (widget-insert "==============================================\n")
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; per-protocol items:
          (mapc 'garak-account-param-widget fields)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (elim-form-widget-create 'push-button
                                   nil
                                   :format (format "[%%[%s%%]]" "Cancel")
                                   :notify 'garak-ui-cancel-cb)
          (widget-insert " ")
          (elim-form-widget-create 'push-button
                                   nil
                                   :format (format "[%%[%s%%]]" "Ok")
                                   :notify 'garak-ui-account-options-ok-cb)
          (use-local-map widget-keymap)
          (widget-setup)
          (beginning-of-buffer)
          (widget-forward 1))
        (display-buffer ui-buffer) )) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buddy list
(defun garak-buddy-list-node-command (&optional widget child event &rest stuff)
  (let (value op buid proc buddy auid bname)
    (setq proc  (cadr (memq :process elim-form-ui-args))
          value (widget-value widget)
          op    (car value)
          buid  (cdr value)
          buddy (elim-buddy-data proc buid)
          bname (elim-avalue "bnode-name"  buddy)
          auid  (elim-avalue "account-uid" buddy))
    (cond ((eq op 'del ) (elim-remove-buddy proc nil  buid))
          ((eq op 'join) (elim-join-chat    proc auid buid))
          ((eq op 'msg ) (elim-message proc auid bname 
                                       (read-string (format "IM %s>" bname))))
          (t (elim-debug "UI Buddy Operation `%S' not implemented" op))) ))

(defun garak-choice-item (tag value)
  (list 'choice-item :format "%[%t%]" :tag tag :value value))
(defun garak-buddy-list-node-widget (proc bnode)
  (let (kids uid menu type name remove plabel)
    (setq uid    (elim-avalue "bnode-uid"  bnode)
          name   (elim-avalue "bnode-name" bnode)
          type   (elim-avalue "bnode-type" bnode)
          remove (list 'choice-item :tag "Remove" :value (cons 'del uid))
          kids   (mapcar 
                  (lambda (N) (garak-buddy-list-node-widget proc N)) 
                  (delq nil (elim-buddy-children proc uid)))
          menu   (cond ((eq type :chat-node )
                        (list (garak-choice-item "Join" (cons 'join uid))))
                       ((eq type :buddy-node)
                        (setq plabel (if (elim-avalue "allowed" bnode) 
                                         "Block" "Unblock"))
                        (list (garak-choice-item "Send IM" (cons 'msg  uid))
                              (garak-choice-item plabel    (cons 'priv uid)))) )
          menu   (cons (garak-choice-item "---------" '(noop)) 
                       (nconc menu (list remove))))
    (if kids
        (apply 'widget-convert 'tree-widget
               :open      t
               :buddy     uid
               :value     uid
               :expander 'garak-buddy-list-node-children
               :node      (apply 'widget-convert 'menu-choice
                                 :format    "%[%t%]\n"
                                 :tag        name
                                 :value      '(noop)
                                 :value-get 'widget-value-value-get
                                 :inline     t
                                 :notify    'garak-buddy-list-node-command 
                                 menu)
               kids)
      (apply 'widget-convert
             'menu-choice
             :format     "%[%t%]\n"
             :tag        name
             :value      '(noop)
             :buddy      uid
             :value-get 'widget-value-value-get
             :inline     t
             :notify    'garak-buddy-list-node-command 
             menu)) ))

(defun garak-buddy-list-skip (proc bnode)
  (if (equal (elim-avalue "contact-size" bnode) 1)
      (progn
        ;;(message "degenerate bnode %s %s -> %s" 
        ;;         (elim-avalue "bnode-name" bnode) 
        ;;         (elim-avalue "bnode-uid"  bnode))
        (or (elim-buddy proc (elim-avalue "contact-main-child-uid" bnode)) 
            bnode)
        )
    bnode))

(defun garak-buddy-list-node-children (widget)
  (let ((uid (widget-get widget :buddy)) children process dummy)
    ;;(message "Updating children for node %S" uid)
    (setq process  (cadr (memq :process elim-form-ui-args))
          children (elim-buddy-children process uid))
    (elim-debug "(garak-buddy-list-node-children %S) -> %S" (car widget) uid)
    (mapcar 
     (lambda (N) 
       (garak-buddy-list-node-widget process 
                                     (garak-buddy-list-skip process N))) 
     children)))

(defun garak-insert-buddy-list-top (proc bnode)
  (let ((uid (elim-avalue "bnode-uid" bnode)) menu name)
    (setq remove (list 'choice-item :tag "Delete All" :value (cons 'del uid))
          name   (elim-avalue "bnode-name" bnode))
    (apply 'widget-create
           'tree-widget
           :open      t
           :tag       name
           :buddy     uid
           :value     uid
           :expander 'garak-buddy-list-node-children
           (mapcar
            (lambda (N)
              (garak-buddy-list-node-widget proc
                                            (garak-buddy-list-skip proc N) ))
            (elim-buddy-children proc (elim-avalue "bnode-uid" bnode))) )))

(defun garak-insert-buddy-list-toplevel (proc bnode)
  (when (not (assoc "bnode-parent" bnode))
    ;;(message "toplevel node: %s" (elim-avalue "bnode-name" bnode))
    (garak-insert-buddy-list-top proc bnode)))

(defun garak-buddy-find-parent (proc uid)
  (let ((buddy (elim-buddy-data proc uid)) parent)
    (setq parent (elim-buddy-data proc (elim-avalue "bnode-parent" buddy)))
    (if (equal (elim-avalue "contact-size" parent) 1)
        (garak-buddy-find-parent proc (car parent))
      (car parent)) ))

(defconst garak-tree-container-classes '(tree-widget-open-icon 
                                         tree-widget-empty-icon
                                         tree-widget-close-icon))

(defun garak-tree-widget-real-target (widget)
  (let ((c (car widget)))
    (when c
      (cond ((eq   c 'tree-widget-leaf-icon      ) (widget-get widget :node  ))
            ((memq c garak-tree-container-classes) (widget-get widget :parent))
            (t widget))) ))

(defun garak-tree-widget-get (widget &optional prop) 
  (when (not prop) (setq prop :value))
  (when widget
    (widget-get (garak-tree-widget-real-target widget) prop)))

(defun garak-tree-widget-set (widget &optional prop value) 
  (when (not prop) (setq prop :value))
  (when widget
    (widget-put
     (garak-tree-widget-real-target widget) prop value)))

(defun garak-tree-widget-apply (widget prop &rest args)
  (when (and widget prop)
    (elim-debug "GARAK-TREE-WIDGET-APPLY %S->%S %S" 
                (car (garak-tree-widget-real-target widget)) 
                (garak-tree-widget-get widget prop)
                args)
    (apply 'widget-apply (garak-tree-widget-real-target widget) prop args)))

(defun garak-buddy-list-find-node (uid)
  (let ((last-point -1) (found nil) (widget nil) (inhibit-point-motion-hooks t))
    (save-excursion
      (beginning-of-buffer)
      (while (and (not found) (< last-point (point)))
        (when (and (setq widget (widget-at))
                   (eql (garak-tree-widget-get (widget-at) :buddy) uid))
          (setq found (cons (point) (car widget))))
        (setq last-point (point))
        (ignore-errors (widget-forward 1))) )
    found))

;;!! update icon of visible node
(defun garak-update-buddy (proc name id status args)
  (let (buid buddy where-widget point widget icon-name icon buffer dummy other)
    (setq buffer (elim-fetch-process-data proc :blist-buffer))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq buid  (elim-avalue    "bnode-uid" args )
              buddy (elim-buddy-data       proc buid )
              other (garak-buddy-list-skip proc buddy))
        ;; if the bnode is not in the "ignored" class, buddy will eq other:
        (when (eq buddy other)
          (setq buid         (elim-avalue "bnode-uid"    args)
                where-widget (garak-buddy-list-find-node buid))
          ;; if where-widget is nil, the bnode is not displayed: find the 
          ;; parent node and clear its cache, and refresh it if it's open:
          (if (not where-widget)
              (let ((puid (garak-buddy-find-parent proc buid)) parent kfun kids)
                (when (setq where-widget (garak-buddy-list-find-node puid))
                  (setq parent (elim-buddy-data proc puid)
                        point  (car where-widget)
                        widget (widget-at  point))
                  (when (and (memq (elim-avalue "bnode-type" parent) 
                                   '(:group-node :contact-node))
                             (garak-tree-widget-get widget :node))
                    (setq kids (garak-tree-widget-apply widget :expander))
                    (garak-tree-widget-set widget :args kids)
                    (when (garak-tree-widget-get widget :open)
                      (widget-button-press point)
                      (widget-button-press point)) )))
            (setq point        (car where-widget)
                  end          (next-single-char-property-change point 'display)
                  widget       (cdr where-widget)
                  icon-name    (garak-buddy-list-choose-icon widget buddy)
                  icon         (tree-widget-find-image icon-name))
            (when icon 
              (let ((inhibit-read-only t))
                ;;(message "adjust display prop %d -> %d : %S" point end icon)
                (put-text-property point end 'display icon) )) )) )) ))
 
(defun garak-delete-buddy (proc name id status args)
  (let (buid puid where-widget point widget buffer dummy)
    (setq buffer (elim-fetch-process-data proc :blist-buffer))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq buid (elim-avalue "bnode-uid"    args)
              puid (elim-avalue "bnode-parent" args))
        (let (parent kfun kids)
          ;; if the parent is skippable, find the first non-skippable ancestor:
          ;; we can't use the normel methods on buddy because it has already 
          ;; been deleted from elim's cache by now: the parent should still
          ;; be alive though as children are reaped before ancestors:
          (setq parent (elim-buddy-data proc puid))
          (when (equal (elim-avalue "contact-size" parent) 1) 
            (setq puid   (garak-buddy-find-parent proc puid)
                  parent (elim-buddy-data proc puid)))
          ;; ok: by now we should have a live ancestor: find its node:
          (when (setq where-widget (garak-buddy-list-find-node puid))
            (setq point  (car where-widget)
                  widget (widget-at  point))
            (when (and (memq (elim-avalue "bnode-type" parent)
                             '(:group-node :contact-node))
                       (garak-tree-widget-get widget :node))
              (setq kids (garak-tree-widget-apply widget :expander))
              (garak-tree-widget-set widget :args kids)
              (when (garak-tree-widget-get widget :open)
                (widget-button-press point)
                (widget-button-press point))) )) )) ))


(defun garak-buddy-list-choose-icon (widget buddy)
  (let ((class (when (consp widget) (car widget) widget)) type)
    (setq type (elim-avalue "bnode-type" buddy))
    (cond ((eq class 'tree-widget-empty-icon) ":invisible")
          ((eq type :chat-node              ) ":chat"     )
          ((eq type :group-node             ) ":group"    )
          ((eq type :contact-node           )
           (setq online (elim-avalue "contact-online-buddies" buddy))
           (if (and online (< 0 online)) ":person" ":off"))
          ((eq type :buddy-node             )
           (symbol-name (elim-avalue "status-type" buddy)))) ))

(defun garak-buddy-list-node-setup-icon (wicon)
  (let ((class   (car wicon))
        (process (cadr (memq :process elim-form-ui-args)))
        buddy buid status icon tag online)
    (when (and (setq buid  (garak-tree-widget-get wicon :buddy))
               (setq buddy (elim-buddy-data process buid )))
      (setq icon (garak-buddy-list-choose-icon wicon buddy))
      (when (and icon (assoc icon garak-icons))
        (if tree-widget-image-enable
            (widget-put wicon :glyph-name icon)
          (when (setq tag (elim-avalue icon garak-icon-tags))
            (widget-put wicon :tag tag)) )) )))

(defun garak-create-ui-widget-buffer (proc) 
  (let ((blist   (elim-buddy-list proc))
        (icons   (copy-sequence garak-icons))
        (bbuffer (elim-fetch-process-data proc :blist-buffer)))
    (when (or (not bbuffer) (not (buffer-live-p bbuffer)))
      (setq bbuffer (generate-new-buffer "*buddy-list*")) 
      (elim-store-process-data proc :blist-buffer bbuffer))
    (with-current-buffer bbuffer
      (elim-init-ui-buffer)
      (tree-widget-set-theme)
      (setq icons (nconc icons (aref tree-widget--theme 3)))
      ;;(message "icons: %S" icons)
      (aset tree-widget--theme 3 icons)
      (add-hook 'tree-widget-before-create-icon-functions 
                'garak-buddy-list-node-setup-icon nil t)
      (setq elim-form-ui-args (list :process proc))
      (mapc
       (lambda (N)
         (garak-insert-buddy-list-toplevel proc N)) blist)
      (use-local-map widget-keymap)
      (widget-setup))
    (display-buffer bbuffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commands
(defun garak-read-username (proc proto)
  (read-string "user name: " nil nil user-login-name t))

(defun garak-read-password (proc proto)
  (read-passwd "password: " t))

(defmacro garak-cmd-strip-account-arg (proc i raw a)
  `(progn
     (setq ,i (split-string ,raw))
     (if (setq ,a (elim-account-data ,proc (car ,i)))
         (setq ,i (cdr ,i)
               ,raw (mapconcat 'identity ,i " "))
       (setq ,a (elim-account-data ,proc garak-account-uid)))
     (car ,a)))

(defun garak-cmd-configure-account (args)
  (let (items account proto buddy errval adata)
    (setq account
          (garak-cmd-strip-account-arg garak-elim-process items args adata))
    (if (not account)
        (format "/configure-account %s: no account found" args)
      (elim-account-options garak-elim-process account 
                            'garak-account-options-ui-cb)
      (format "/configure-account %s" args)) ))

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
  (let (items account proto buddy errval adata group)
    (setq account
          (garak-cmd-strip-account-arg garak-elim-process items args adata)
          buddy (car  items)
          group (cadr items))
    (condition-case errval
        (progn
          (elim-add-buddy garak-elim-process account buddy group)
          (format "/add-buddy %s %s %S" buddy group items))
        (error "Could not add buddy: %S" errval)) ))

(defun garak-cmd-remove-buddy (args)
  (let (items account account-uid)
    (setq account-uid
          (garak-cmd-strip-account-arg garak-elim-process items args account))
    
    (format "/remove-buddy %s %s" 
            (elim-avalue "account-name" (cdr account)) 
            (car items)) ))

(defun garak-cmd-msg (args)
  (let (items account account-data proto buddy a-end message rval)
    (setq rval (format "INVALID: /msg %s" args))
    (setq account
          (garak-cmd-strip-account-arg garak-elim-process 
                                       items args account-data))
    (when (string-match "\\s-*\\(\\S-+\\)\\s-+" args)
      (setq buddy   (match-string 1 args)
            message (substring args (match-end 0)))
        (elim-message garak-elim-process account buddy message)
        (setq rval (format "/msg %s" args)) ) 
    rval))

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
    (setq rval    (format "/join %s (failed)" args)
          account (garak-cmd-strip-account-arg garak-elim-process 
                                               items args account-data))
    (if account-data
        (progn
          (setq proto (cdr (assq :proto (cdr account-data)))
                spec  (elim-chat-parameters garak-elim-process proto))
          ;;(message "arg-spec: %S" spec)
          (if spec 
              (if (setq options (garak-read-join-parameters spec items))
                  (progn 
                    ;;(message "e-j-c: process %S %S %S" account "" options)
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

(defun garak-cmd-account-generic (cmd elim-op args)
  (let ( (account-data 
          (or (elim-account-data garak-elim-process args)
              (elim-account-data garak-elim-process garak-account-uid))) )
    (if (not account-data)
        (format "/%s %s: no account found" cmd args)
      (funcall elim-op garak-elim-process (car account-data))
      (format "/%s %s" cmd args)) ))

(defun garak-cmd-connect    (args)
  (garak-cmd-account-generic "connect"        'elim-connect        args))
(defun garak-cmd-register   (args)
  (garak-cmd-account-generic "register"       'elim-register       args))
(defun garak-cmd-unregister (args)
  (garak-cmd-account-generic "unregister"     'elim-unregister     args))
(defun garak-cmd-remove-account (args)
  (garak-cmd-account-generic "remove-account" 'elim-remove-account args))
(defun garak-cmd-disconnect (args)
  (garak-cmd-account-generic "disconnect"     'elim-disconnect     args))

(defun garak-cmd-not-implemented (args)
  "Command not implemented")

(defvar garak-command-handlers
  '((add-account  . garak-cmd-add-account      )
    (add-buddy    . garak-cmd-add-buddy        )
    (config-acct  . garak-cmd-configure-account)
    (connect      . garak-cmd-connect          )
    (disconnect   . garak-cmd-disconnect       )
    (help         . garak-cmd-help             )
    (join         . garak-cmd-join             )
    (leave        . garak-cmd-leave            )
    (msg          . garak-cmd-msg              )
    (quit         . garak-cmd-quit             )
    (register     . garak-cmd-register         )
    (remove-acct  . garak-cmd-remove-account   )
    (remove-buddy . garak-cmd-remove-buddy     )
    (unregister   . garak-cmd-unregister       ) ))

(defun garak-command-match (cmd)
  (cond
   ((string-match "\\(?:^\\|/\\)add.account\\>"         cmd) 'add-account )
   ((string-match "\\(?:^\\|/\\)add.buddy\\>"           cmd) 'add-buddy   )
   ((string-match "\\(?:^\\|/\\)configure.account\\>"   cmd) 'config-acct )
   ((string-match "\\(?:^\\|/\\)configure\\>"           cmd) 'config-acct )
   ((string-match "\\(?:^\\|/\\)connect\\>"             cmd) 'connect     )
   ((string-match "\\(?:^\\|/\\)login\\>"               cmd) 'connect     )
   ((string-match "\\(?:^\\|/\\)disconnect\\>"          cmd) 'disconnect  )
   ((string-match "\\(?:^\\|/\\)logout\\>"              cmd) 'disconnect  )
   ((string-match "\\(?:^\\|/\\)logoff\\>"              cmd) 'disconnect  )
   ((string-match "\\(?:^\\|/\\)\\(?:help\\>\\|\\?\\)"  cmd) 'help        )
   ((string-match "\\(?:^\\|/\\)join\\(?:.\\S-+\\)?\\>" cmd) 'join        )
   ((string-match "\\(?:^\\|/\\)part\\>"                cmd) 'leave       )
   ((string-match "\\(?:^\\|/\\)leave\\>"               cmd) 'leave       )
   ((string-match "\\(?:^\\|/\\)\\(?:priv\\)?msg\\>"    cmd) 'msg         )
   ((string-match "\\(?:^\\|/\\)register\\>"            cmd) 'register    )
   ((string-match "\\(?:^\\|/\\)remove.account\\>"      cmd) 'remove-acct )
   ((string-match "\\(?:^\\|/\\)remove.buddy\\>"        cmd) 'remove-buddy)
   ((string-match "\\(?:^\\|/\\)quit\\>"                cmd) 'quit        )
   ((string-match "\\(?:^\\|/\\)unregister\\>"          cmd) 'unregister  ) ))

(defun garak-command-handler (cmd args &optional raw)
  (let (command handler)
    (setq command (garak-command-match cmd)
          handler (cdr (assq command garak-command-handlers)))
    (elim-debug "CMD HANDLER: %S" handler)
    (if (fboundp handler)
        (funcall handler (or args ""))
      (when (and raw (not handler)) ;; native / command?
        (elim-do-cmd garak-elim-process garak-account-uid 
                     garak-conv-uid (concat cmd " " args)) nil)) ))

(defun garak-input-handler (input)
  (let ((output input) (command nil) (args nil))
    (if (string-match "^/\\(\\S-+\\)\\s-*\\(.*\\)" input)
        (setq command (match-string 1 input)
              args    (match-string 2 input)
              output  (garak-command-handler command args input))
      (elim-debug "COMMAND: %S -> %S" command output)
      (set-text-properties 0 (length output) nil output)
      (elim-message garak-elim-process garak-account-uid garak-conv-uid output)
      (setq output nil))
    (when output (lui-insert output)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion:
(defvar garak-commands 
  '( "/add-account"    "/add-buddy"    "/configure-account" "/connect"
     "/disconnect"     "/login"        "/logoff"            "/logout"  
     "/msg"            "/quit"         "/register"
     "/remove-account" "/remove-buddy" "/unregister" ))

(defvar garak-command-completers 
  '((add-account . garak-comp-add-account)
    (add-buddy   . garak-comp-add-buddy  )
    (msg         . garak-comp-msg        )
    (connect     . garak-comp-account    )
    (config-acct . garak-comp-account    )
    (disconnect  . garak-comp-account    )
    (register    . garak-comp-account    )
    (remove-acct . garak-comp-account    )
    ()
    (unregister  . garak-comp-account    )
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

(defun garak-comp-account (prefix &optional protocol)
  (garak-comp-add-buddy prefix))

(defun garak-comp-msg         (prefix &optional protocol) 
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

(defun garak-buddy-list ()
  "Create and display the garak buddy list"
  (interactive)
  (garak-create-ui-widget-buffer garak-elim-process))

(defun garak ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "*garak*"))
  (garak-mode)
  (setq lui-input-function 'garak-input-handler)
  (lui-insert "starting elim" t)
  (set (make-local-variable 'garak-elim-process) 
       (elim-start "garak" nil garak-callbacks))
  (garak-create-ui-widget-buffer garak-elim-process)
  (end-of-buffer))

(provide 'garak)