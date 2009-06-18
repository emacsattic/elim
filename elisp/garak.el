;; Copyright © 2009 Vivek Dasmohapatra 

;; email : vivek@etla.org
;; irc   : fledermaus on freenode, oftc
;; jabber: fledermaus@jabber.earth.li

;; This file is part of elim.

;; elim is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; elim is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with elim.  If not, see <http://www.gnu.org/licenses/>.
(require 'lui   nil t)
(require 'elim  nil t)
(require 'widget     )
(require 'tree-widget)
(require 'time-date  )
(require 'image      )

(let ((load-path (cons (file-name-directory
                        (or load-file-name buffer-file-name)) load-path)))
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
;; user customisation
(defconst garak-icon-directory-internal
  (expand-file-name
   (concat (file-name-directory
            (or load-file-name buffer-file-name)) "../icons/")))

(defcustom garak-icon-directory garak-icon-directory-internal
  "Directory where garak may find icons."
  :type  '(directory)
  :group 'garak)

(defcustom garak-hide-offline-buddies nil
  "Whether or not to conceal offline buddies"
  :type  '(boolean)
  :group 'garak)

(defun garak-tag-widget-value (w)
  (let ((raw (widget-field-value-get w)))
    (cond ((eq (length raw) 2)         (concat     raw " "))
          ((eq (length raw) 1)         (concat " " raw " "))
          (t raw)) ))
(defcustom garak-icon-tags '((":available"      . "[i]")
                             (":away"           . "[_]")
                             (":blocked"        . "(X)")
                             (":busy"           . "(/)")
                             (":chat"           . "ii ")
                             (":connecting"     . " * ")
                             (":extended-away"  . " - ")
                             (":garak"          . "Gª ")
                          ;; (":group"          . "iii")
                             (":invisible"      . "   ")
                             (":log-in"         . "...")
                             (":log-out"        . " - ")
                             (":offline"        . " x ")
                             (":off"            . " x ")
                             (":on"             . " ! ")
                             (":person"         . " i ")
                             (":unavailable"    . "(/)")
                             ;; protocol icon tags:
                             (":prpl-aim"       . "AIM ")
                             (":prpl-bonjour"   . "Bon ")
                             (":prpl-gg"        . "GG  ")
                             (":prpl-icq"       . "ICQ ")
                             (":prpl-irc"       . "IRC ")
                             (":prpl-jabber"    . "XMPP")
                             (":prpl-meanwhile" . "Mnw ")
                             (":prpl-msn"       . "MSN ")
                             (":prpl-novell"    . "Novl")
                             (":prpl-qq"        . " QQ ")
                             (":prpl-silc"      . "SILC")
                             (":prpl-simple"    . "Smpl")
                             (":prpl-yahoo"     . " Y! ")
                             (":prpl-zephyr"    . "Zeph"))
  "An alist of icon names and the alternate text used to represent them when
images are unavailable. When in a unicode-capable text environment, I like to
substitute these characters for the basic ascii ones:\n
  :available       ♝
  :away            ∠
  :blocked         ⊗
  :busy            ⊘
  :chat            ♗♝
  :connecting      ≔
  :extended-away   ≣
  :garak           Gª
  :invisible       ♗
  :log-in          …
  :log-out         ⋮
  :offline         ·
  :off             ×
  :on              o
  :person          ♟
  :unavailable     ⊘\n"
  :group   'garak
  :tag     "icon tags"
  :options (mapcar
            (lambda (k)
              (list (list 'string :value k :size 20 :format "Icon: %v ")
                    (list 'string :size  6 :format "Tag: %v\n"
                          :value-get 'garak-tag-widget-value)))
            '(":available"   ":away"         ":busy"           ":blocked"
              ":chat"        ":connecting"   ":extended-away"  ":garak"
              ":invisible"   ":log-in"       ":log-out"        ":offline"
              ":off"         ":on"           ":person"         ":unavailable"
              ":prpl-aim"    ":prpl-bonjour" ":prpl-gg"        ":prpl-icq"    
              ":prpl-irc"    ":prpl-jabber"  ":prpl-meanwhile" ":prpl-msn"  
              ":prpl-novell" ":prpl-qq"      ":prpl-silc"      ":prpl-simple"
              ":prpl-yahoo"  ":prpl-zephyr"))
  :type     '(alist :key-type   (string :format "Icon: %v " :size 20)
                    :value-type (string :format "Tag: %v\n" :size  6)))


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
(defun garak-file-attr-to-icon (attr)
  (when (or (stringp (cadr attr))
            (null    (cadr attr)))
    (let ((label (concat ":" (file-name-sans-extension (car attr))))
          (path  (concat garak-icon-directory (car attr))))
      (cons label
            (ignore-errors (create-image path nil nil
                                         :pointer 'hand
                                         :ascent  'center
                                         :mask '(heuristic t)))) )))
(defun garak-load-icons ()
  (when (and (not garak-icons) (file-directory-p garak-icon-directory))
    (setq garak-icons
          (delq nil
                (mapcar 'garak-file-attr-to-icon
                        (directory-files-and-attributes garak-icon-directory))))
    (length garak-icons)))

(defvar garak-icons nil
  "Alist of names and icons \(images, see `create-image'\) to use in the gui")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; callback lookup:
(defvar garak-callbacks
  '(;;(elim-account-notify-added)
    (elim-account-status-changed . garak-account-update      )
    (elim-account-request-add    . garak-stranger-added-you  )
    (elim-account-request-auth   . garak-auth-requested      )
    ;; blist (buddy list) ops
    ;;(elim-blist-create-node                                )
    (elim-blist-update-node      . garak-update-buddy        )
    (elim-blist-remove-node      . garak-delete-buddy        )
    (elim-blist-request-add-buddy . garak-request-add-buddy  )
    ;; connection ops
    (elim-connection-state       . garak-account-update      )
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
    ;; accounts:
    (add-account                 . garak-account-update      )
    (remove-account              . garak-account-update      )
    ;; prefs
    (get-prefs                   . garak-setup-prefs-buffer  )
    ;; file transfers
    (elim-file-transfer-status   . garak-transfer-status     )
    (elim-file-transfer-percent  . garak-transfer-percent    )
    ;; notify
    (elim-notify-message         . garak-notify-message      )
    (elim-notify-formatted       . garak-notify-formatted    )
    (elim-notify-email           . garak-notify-email        )
    (elim-notify-emails          . garak-notify-emails       )
    (elim-notify-search          . garak-notify-search       )
    (elim-notify-search-rows     . garak-notify-search-rows  )
    (elim-notify-userinfo        . garak-notify-userinfo     )
    (elim-notify-uri             . garak-notify-uri          )
    ;; messages:
    (elim-conv-write-chat        . garak-chat-message        )
    (elim-conv-write-im          . garak-user-message        )
    (elim-conv-write-sys         . garak-misc-message        )
    ;; process related
    (elim-exit                   . garak-elim-exit           )
    ;; failsafe error handler     
    (error                       . garak-error-handler       ))
  "Alist of elim callbacks and their corresponding handlers")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; error handling
(defun garak-error-handler (proc name id status error)
  (let ((message (elim-add-face 
                  (format "%S: %s" name error) 'garak-warning-face))
        (target  (window-buffer))
        (buf      nil)
        (mode    'garak-mode)
        buffers)
    (if (and (eq mode major-mode)
             (eq (buffer-local-value 'garak-elim-process target) proc))
        (lui-insert message)
      (when (and (setq target (elim-fetch-process-data proc :cli-buffer))
                 (eq mode major-mode)
                 (eq (buffer-local-value 'garak-elim-process target) proc))
        (with-current-buffer target (lui-insert message)) )) ))

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

(defun garak-buffer-reusable (proc buffer)
  (let (oldproc)
    (and (bufferp       buffer)
         (buffer-live-p buffer)
         (setq oldproc (buffer-local-value 'garak-elim-process buffer))
         (let ((status (and (processp oldproc) (process-status oldproc))))
           (or (eq oldproc proc)
               (not (memq status '(run stop open connect))) )) )))

(defun garak-conversation-buffer (proc args &optional do-not-create)
  (let ((uid   (elim-avalue "conv-uid"  args))
        (cname (elim-avalue "conv-name" args)) buffer)
    (when uid
      (setq buffer (or (elim-avalue uid garak-conversation-buffers) 
                       (get-buffer cname))))
    (when (and (bufferp buffer) (not (buffer-live-p buffer)))
      (rassq-delete-all buffer garak-conversation-buffers)
      (setq buffer nil))
    (if (not (garak-buffer-reusable proc buffer))
        (if do-not-create
            (setq buffer nil)
          (setq buffer (generate-new-buffer cname)
                garak-conversation-buffers
                (cons (cons uid buffer) garak-conversation-buffers)) ))
    buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; process tracking and management

(defun garak-elim-exit (proc name id status args)
  (mapc (lambda (acct) (garak-account-update proc name id 0 (list acct)))
        (mapcar
         (lambda (A) (cons "account-uid" (car A)))
         (elim-fetch-process-data proc :accounts)))
  (mapc
   (lambda (buffer)
     (with-current-buffer buffer
       (when (and (eq 'garak-mode  major-mode) 
                  (eq garak-elim-process proc)) 
         (let ((message (format "* elim process finished: %S *" args)))
           (lui-insert (elim-add-face message 'garak-warning-face)) )) ))
   (buffer-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; notifications:
(defun garak-notice-buffer (proc) 
  (let ((nbuf (elim-fetch-process-data proc :notice-buffer)))
    (when (not (buffer-live-p nbuf))
      (setq nbuf (get-buffer "*garak-notices*"))
      (when (not (garak-buffer-reusable proc nbuf))
        (setq nbuf (generate-new-buffer "*garak-notices*")))
      (with-current-buffer nbuf
        (elim-init-ui-buffer)
        (garak-init-local-storage)
        (setq garak-elim-process proc)
        (use-local-map widget-keymap)
        (elim-store-process-data proc :notice-buffer nbuf)))
    nbuf))

(defvar garak-image-cache nil)

(defun garak-cache-image (proc name id attr args)
  (when (equal (elim-avalue "status" args) 0)
    (let ((image (elim-avalue "value" args)) iid data)
      (setq iid  (elim-avalue "image-id"   image)
            data (elim-avalue "image-data" image))
      (assq-delete-all iid garak-image-cache)
      (setq garak-image-cache (cons (cons iid data) garak-image-cache))
      data)))

(defun garak-interpret-image-markup (process string)
  (let ((pos                 0) 
        (case-fold-search    t) 
        (counter             0) 
        (garak-image-cache nil) ids id data image start end)
    (while (string-match "<img\\s-+id='\\([0-9]+\\)'>" string pos)
      (setq id  (string-to-int (match-string 1 string))
            pos (match-end 0))
      (add-to-list 'ids id nil 'eq))
    (if (display-images-p)
        (progn
          (mapc 
           (lambda (i) 
             (elim-image process i 'garak-cache-image)
             (mapc (lambda (x) 
                     (accept-process-output process 0 50 1)) '(0 1 2))) ids)
          (setq pos 0)
          (while (string-match "<img\\s-+id='\\([0-9]+\\)'>" string pos) 
            (setq pos   (match-end 0)
                  start (match-beginning 0)
                  end   (match-end       0)  
                  id    (match-string 1 string)
                  id    (string-to-int id)
                  data  (cdr (assq id garak-image-cache))
                  image (when data (create-image data nil t)))
            (when (and data image)
              (put-text-property start end 'display image string)))) 
      (while (string-match "<img\\s-+id='\\([0-9]+\\)'>" string pos) 
        (setq pos (match-end 0))
        (put-text-property (match-beginning 0) 
                           (match-end       0) 'display "[ICON]" string)))
    string))

(defun garak-kill-notice (&optional widget child event &rest stuff)
  (let ((value (widget-value widget)) 
        (sprop 'garak-notice-start)
        (eprop 'garak-notice-close)
        (proc   garak-elim-process)
        response name id start end pos)
    (setq name     (nth 0 value)
          id       (nth 1 value)
          response (elim-daemon-response name id 0 (elim-atom-to-proto nil)))
    ;; track down the start and end of this notice:
    (when (equal (get-text-property (point-min) sprop) id)
      (setq start (point-min)))
    (setq pos (point-min))
    (while (and (not start)
                (< pos (point-max))
                (setq pos (next-single-char-property-change pos sprop nil)))
      (when (equal (get-text-property pos sprop) id)
        (setq start pos)))
    (while (and (not end)
                (< pos (point-max))
                (setq pos (next-single-char-property-change pos eprop nil)))
      (when (equal (get-text-property pos eprop) id)
        (setq end pos)))
    ;;(elim-debug "CHECKING ID: %S" id)
    ;;(elim-debug "ERASE: %S to %S in %S" start end (current-buffer))
    ;;(elim-debug "SEND : %S" response)
    (elim-process-send proc response)
    (when (and  start  end) (delete-region start (1+ end)))
    (when (< (point-max) 2) (bury-buffer)) ))

(defun garak-begin-notice (id)
  (widget-insert (propertize "+---------\n" 'garak-notice-start id)))

(defun garak-end-notice (name id)
  (widget-create 'push-button
                 :format "%[[Ok]%]" 
                 :notify 'garak-kill-notice
                 :value  (list name id))
  (widget-insert (propertize "\n" 'garak-notice-close id)))

(defalias 'garak-notify-message 'garak-notify-formatted)
(defun garak-notify-formatted (proc name id status args)
  (let ((nbuf  (garak-notice-buffer proc))
        (mface 'garak-marker-face)
        (title (or (elim-avalue "title" args) "-Notice-"))
        (h1    (elim-avalue "primary"   args))
        (h2    (elim-avalue "secondary" args))
        (text  (elim-avalue "text"      args)))
    (with-current-buffer nbuf
      (garak-begin-notice id)
      (widget-insert (propertize title 'face 'garak-warning-face))
      (widget-insert "\n")
      (when h1   (widget-insert (propertize h1 'face mface) ":\n"))
      (when h2   (widget-insert (propertize h2 'face mface) ":\n"))
      (when text (widget-insert text))
      (garak-end-notice name id)) 
    (let ((display-buffer-reuse-frames t)) 
      (display-buffer nbuf)) ))

(defun garak-notify-email (proc name id status args)
  (let ((nbuf  (garak-notice-buffer proc))
        (mface 'garak-marker-face)
        (title (or (elim-avalue "subject" args) "-email-"))
        (url   (elim-avalue "url" args))
        (from  (or (elim-avalue "from" args) "?"))
        (to    (or (elim-avalue "to"   args) "?")))
    (setq from (propertize from 'face 'garak-nick-face    ))
    (setq to   (propertize to   'face 'garak-own-nick-face))
    (with-current-buffer nbuf
      (garak-begin-notice id)
      (widget-insert (propertize title 'face mface) "\n")
      (when url (widget-insert (propertize url 'face mface) ":\n"))
      (widget-insert (format "%s -> %s" from to) "\n")
      (garak-end-notice name id)) 
    (let ((display-buffer-reuse-frames t))
      (display-buffer nbuf)) ))

(defun garak-notify-uri (proc name id status args)
  (let ((nbuf  (garak-notice-buffer proc))
        (mface 'garak-marker-face)
        (url   (elim-avalue "url" args)))
    (with-current-buffer nbuf
      (garak-begin-notice id)
      (widget-insert (propertize url 'face mface) "\n")
      (garak-end-notice name id)) 
    (let ((display-buffer-reuse-frames t))
      (display-buffer nbuf)) ))

(defun garak-notify-userinfo (proc name id status args)
  (let ((nbuf  (garak-notice-buffer proc))
        (mface 'garak-marker-face)
        (l-len  5)
        (l-fmt "%%-%ds : %%s\n")
        (bname (elim-avalue "user-name" args))
        (info  (elim-avalue "user-info" args)))
    (with-current-buffer nbuf
      (garak-begin-notice id)
      (widget-insert (propertize bname 'face 'garak-nick-face) "\n")
      (mapc 
       (lambda (entry) 
         (let ((label (car entry)))
           (when (and (stringp label)
                      (not (equal  label "-"))
                      (<   (length label) 20)
                      (>   (length label) l-len))
             (setq l-len (length label)) ))) info)
      (setq l-fmt (format l-fmt l-len))
      (mapc
       (lambda (entry)
         (let ((label (car entry))
               (type  (elim-avalue "type"  (cdr entry)))
               (value (elim-avalue "value" (cdr entry))))
           (if value
               (setq value (elim-interpret-markup value)
                     value (garak-interpret-image-markup proc value))
             (setq value ""))
           (cond ((eq type :section-break)
                  (widget-insert "---------------------------------------\n"))
                 ((eq type :section-header)
                  (widget-insert 
                   (format "%s "  (propertize label 'face 'garak-marker-face))
                   (format "%s\n" (propertize value 'face 'garak-marker-face))))
                 ((eq type :pair)
                  (if (not (equal "-" label))
                      (widget-insert (format l-fmt label value)) 
                    (widget-insert value)) )) )) info)
      (garak-end-notice name id)) 
    (let ((display-buffer-reuse-frames t))
      (display-buffer nbuf)) ))

(defvar garak-search-button-menu nil)
(defun garak-search-results-buffer (proc search-id args) 
  (let ((i 0) sbuf sbuf-store)
    (setq sbuf-store (elim-fetch-process-data proc :search-buffers)
          sbuf       (elim-avalue search-id sbuf-store))
    (when (and sbuf (not (buffer-live-p sbuf)))
      ;;(message "detroying dead s-buffer")
      (setq sbuf-store (rassq-delete-all sbuf sbuf-store)
            sbuf nil))
    (when (not sbuf)
      (setq sbuf (generate-new-buffer (or (elim-avalue "title"   args)
                                          (elim-avalue "primary" args)
                                          "*Search Results*"         ))
            sbuf-store (cons (cons search-id sbuf) sbuf-store))
      (with-current-buffer sbuf
        (elim-init-ui-buffer)
        (garak-init-local-storage)
        (setq elim-form-ui-args  args)
        (setq garak-elim-process proc)
        (setq garak-account-uid (elim-avalue "account-uid" args))
        (set (make-local-variable 'garak-search-button-menu)
             (make-sparse-keymap "Actions..."))
        (mapc 
         (lambda (a) 
           (let (cb action label)
             (setq label  (car a)
                   action (elim-avalue "action" (cdr a))
                   cb    `(lambda (e &rest x)
                            (interactive (list (this-command-keys)))
                            (when (vectorp e) (setq e (aref e 0)))
                            (setq e
                                  (if (eventp e) 
                                      (posn-point (event-start e)) 
                                    (point)))
                            (garak-notify-search-callback e ,action)))
             (define-key garak-search-button-menu (format "%d" i)
               (list 'menu-item label cb))) 
           (setq i (1+ i)))
         (elim-avalue "buttons" args)))
      (elim-store-process-data proc :search-buffers sbuf-store))
    sbuf))

(defvar garak-search-column-labels nil)
(defun garak-notify-search-callback (point action)
  (message "garak-notify-search-callback(%S %S)" point action)
  (let ((row nil) (start point) (args nil))
    (setq start (next-single-char-property-change (point-min) 'result-start)
          row   (- (line-number-at-pos point) 
                   (line-number-at-pos start) 2)
          args  (elim-simple-list-to-proto-alist 
                 (list "search-id"   (elim-avalue "search-id" elim-form-ui-args)
                       "account-uid" garak-account-uid
                       "row-index"   row
                       "callback"    action))
          call  (elim-daemon-call 'notify-search-callback nil args)) 
    (elim-process-send garak-elim-process call) ))

(defun garak-notify-search (proc name id status args)
  (with-current-buffer (garak-search-results-buffer proc id args)
    (erase-buffer)
    (setq elim-form-ui-args args)
    (mapc (lambda (x)
            (when (and (> (length x) 0) (stringp x)) (insert x "\n"))) 
          (mapcar (lambda (k) (elim-avalue k args))
                  '("title" "primary" "secondary")))
    (insert (propertize "\n" 'result-start t))
    (set (make-local-variable 'garak-search-column-labels) 
         (elim-avalue "columns" args)) ))

(defun garak-notify-search-rows (proc name id status args)
  (message "garak-notify-search-rows(%s...)" id)
  (let ((sid   (elim-avalue "search-id" args))
        (rows  (elim-avalue "results"   args))
        (titles nil)
        (widths nil)
        (format nil)
        (menu   nil)
        (rrow   nil)
        (l      0)
        (n      0))
    (with-current-buffer (garak-search-results-buffer proc sid args)
      (setq titles garak-search-column-labels
            widths (make-vector (length titles) 0))
      (delete-region 
       (1+ (previous-single-char-property-change (point-max) 'result-start))
       (point-max))
      (mapc (lambda (row)
              (setq n 0)
              (mapc (lambda (cell) 
                      (setq l (length cell))
                      (when (< (aref widths n) l) (aset widths n l))
                      (setq n (1+ n))) row))
            (cons titles rows))
      (setq format (mapconcat (lambda (x) (format "%%%ds" x)) widths " | "))
      (setq menu (if (current-local-map)
                     (copy-keymap (current-local-map))
                   (make-sparse-keymap)))
      (mapc (lambda (key) 
              (define-key menu key garak-search-button-menu))
            (list (kbd "RET") (kbd "<down-mouse-1>")))
      (insert (apply 'format format titles) "\n")
      (mapc
       (lambda (x) 
         (insert (propertize (apply 'format format x) 'keymap menu) "\n")) rows)
      (display-buffer (current-buffer))) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; message callbacks
(defun garak-abbreviate-nick (nick &optional protocol)
  (if (string-match "^\\(.*?\\)@" nick) (match-string 1 nick) nick))

(defun garak-command-response (process call call-id status args)
  (let ((buffer (garak-conversation-buffer process args t)) cmd cstatus err)
    ;;(message "garak-command-response %S %S %S" call status args)
    (if buffer
      (with-current-buffer buffer
        (if (not (zerop status))
            (garak-error-handler process call call-id status args)
          (setq cmd     (elim-avalue "command-line"   args)
                cstatus (elim-avalue "command-status" args)
                err     (or (elim-avalue "command-error"  args)
                            (elim-avalue "command-status" args)))
          (if (not (eq :ok cstatus))
              (lui-insert (elim-add-face (format "/%s: %s" cmd err)
                                         'garak-warning-face))
            (lui-insert (elim-add-face (format "/%s" cmd)
                                       'garak-system-message-face))) ))
      (warn "%S response %s %S: no target buffer" call call-id args)) ))

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
    ;;(message "dlabel: %s" dlabel)
    (when (< 3600 delay)
      (setq handled (round delay 3600.0)
            delay   (mod delay 3600)
            ilabel  (format "%d hour%s" handled   (if (< 1 handled) "s" ""))
            dlabel  (concat dlabel (when dlabel " ") ilabel)))
    ;;(message "dlabel: %s" dlabel)
    (when (< 60 delay)
      (setq handled (round delay 60.0)
            delay   (mod delay 60)
            ilabel  (format "%d minute%s" handled (if (< 1 handled) "s" ""))
            dlabel  (concat dlabel (when dlabel " ") ilabel)))
    ;;(message "dlabel: %s" dlabel)
    (when (< 0 delay)
      (setq ilabel (format "%d second%s" delay    (if (< 1 delay  ) "s" ""))
            dlabel (concat dlabel (when dlabel " ") ilabel)))
    ;;(message "dlabel: %s" dlabel)
    (format "%s (%s ago)" absolute dlabel)))

(defun garak-chat-message (process call call-id status args)
  (let ( (buffer  (garak-conversation-buffer process args t))
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
  (let ((buffer (garak-conversation-buffer process args t)))
    (when (not buffer)
      (setq buffer (garak-conversation-buffer process args))
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
  (let ((buffer (garak-conversation-buffer process args t)))
    (when buffer
      (with-current-buffer buffer
        (setq garak-dead-conversation-buffers
              (cons buffer garak-dead-conversation-buffers)
              garak-conversation-buffers
              (rassq-delete-all buffer garak-conversation-buffers))
        (lui-insert "*conversation ended*") )) ))

(defun garak-chat-add-users (process call call-id status args)
  (let ((buffer (garak-conversation-buffer process args t))
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

(defun garak-choice-item (tag &optional value)
  (if value
      (list 'choice-item :format "%[%t%]" :tag tag :value value)
    (list 'choice-item :value tag)))

(defun garak-ui-cancel-cb (&optional parent child event &rest stuff)
  (when elim-form-ui-args (kill-buffer nil)))

(defun garak-menu-choice-mouse-down-action (widget &optional event)
  (let ((args (widget-get widget :args  ))
	(old  (widget-get widget :choice)))
    (cond ((not (display-popup-menus-p))              nil)     ;; no popups
	  ((> (length args) widget-menu-max-size)     nil)     ;; list too long
	  ((> (length args) 2)                          t)     ;; use menu
	  ((and widget-choice-toggle (memq old args)) nil)     ;; toggle
	  (t                                            t)) )) ;; ask

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
;; preferences buffer
(defun garak-ui-create-prefs-buffer (proc) 
  (let ((buffer (or (elim-fetch-process-data proc :prefs-buffer) 
                    (get-buffer "*garak-prefs**"))))
    (when (not (garak-buffer-reusable proc buffer))
      (setq buffer (generate-new-buffer "*garak-prefs*"))
      (elim-store-process-data proc :prefs-buffer buffer))
    (with-current-buffer buffer 
      (elim-init-ui-buffer)
      (garak-init-local-storage)
      (setq garak-elim-process proc)
      (tree-widget-set-theme)
      (use-local-map widget-keymap)
      (elim-get-prefs proc 'garak-ui-prefs-insert-widget)) buffer))

(defun garak-ui-pref-to-node (pref)
  (let (type name kids data val w-args node)
    (setq data   (cdr pref)
          name   (car (last (split-string (car pref) "/" nil)))
          type   (elim-avalue "pref-type"    data)
          val    (elim-avalue "pref-value"   data)
          choice (elim-avalue "pref-choices" data))
    (if choice
        (setq w-args 
              (apply 'list      'choice 
                     :value      val
                     :format    "%t: %[%v%]\n"
                     :value-get 'widget-value-value-get
                     (mapcar 
                      (lambda (p) 
                        (list 'const :format "%t" :tag (car p) (cdr p))) 
                      choice)))
      (setq w-args
            (cond 
             ((eq type :boolean) (list 'toggle    :format "%t: %[%v%]\n" val))
             ((eq type :string ) (list 'string    (or val "")))
             ((eq type :path   ) (list 'directory (or val "")))
             ((eq type :int    ) (list 'number    (or val 0 ))) 
             (t                  (list 'const     :value val)))))
    (apply 'widget-convert
           (car w-args)
           :tag        (format "%-16s" name)
           :old-value  val  
           :garak-pref (car pref)
           (cdr w-args)) ))

(defun garak-ui-pref-significant-widgets ()
  (let ((last-point -1) wlist widget)
    (save-excursion 
      (beginning-of-buffer)
      (while (< last-point (point))
        (when (and (setq widget (widget-at)) 
                   (widget-get widget :garak-pref))
          (setq wlist (cons widget wlist)))
        (setq last-point (point))
        (ignore-errors (widget-forward 1)))
      wlist) ))

(defun garak-pref-equal (old new)
  (if (not old)
    (cond ((stringp  new) (and (equal "" new) t))
          ((numberp  new) (and (zerop    new) t))
          (t (equal old new)))
    (equal old new)))

(defun garak-ui-prefs-changed ()
  (let (pref)
    (delq nil
          (mapcar 
           (lambda (w)
             (when (setq pref (widget-get w :garak-pref))
               (let ((old-value (widget-get   w :old-value)) 
                     (new-value (widget-value w)))
                 (and (not (garak-pref-equal old-value new-value))
                      (cons pref new-value)) )))
           (garak-ui-pref-significant-widgets))) ))

(defun garak-ui-pref-node-command (&optional widget child event &rest args)
  (message "garak-ui-pref-node-command(%S %S %S %S)" 
           (car widget) (car child) event args))

(defun garak-ui-pref-to-widget (pref)
  (apply 'widget-convert 'tree-widget
         :open       t
         :help-echo  nil
         :node       (garak-ui-pref-to-node pref)
         :inline     t
         (garak-ui-pref-children pref)))

(defun garak-ui-pref-children (pref)
  (let (kids)
    (setq kids (elim-avalue "pref-children" (cdr pref)))
    (mapcar 'garak-ui-pref-to-widget kids)))

(defun garak-ui-prefs-process-widgets (&optional parent child event &rest stuff)
  (let ((actions (widget-value parent)) 
        (proc     garak-elim-process))
    (message "UI Prefs Button: %S" actions)
    (when (memq 'save   actions) (elim-set-prefs proc (garak-ui-prefs-changed)))
    (when (memq 'reload actions) (garak-ui-create-prefs-buffer proc))
    (when (memq 'close  actions) (kill-buffer nil)) ))

(defun garak-ui-prefs-insert-buttons ()
  (widget-create 'push-button
                 :format "%[[%t]%]"
                 :tag    "Save Prefs"
                 :notify 'garak-ui-prefs-process-widgets
                 :value  '(save reload))
  (widget-insert " ")
  (widget-create 'push-button
                 :format "%[[%t]%]"
                 :tag    "Save and Close Editor"
                 :notify 'garak-ui-prefs-process-widgets
                 :value  '(save close))
  (widget-insert " ")
  (widget-create 'push-button
                 :format "%[[%t]%]"
                 :tag    "Close Prefs Editor"
                 :notify 'garak-ui-prefs-process-widgets
                 :value  '(close))
  (widget-insert "\n"))

(defun garak-ui-prefs-insert-widget (proc name id attr args)
  (let ((buffer (elim-fetch-process-data proc :prefs-buffer)) prefs) 
    (when buffer
      (with-current-buffer buffer
        (garak-ui-prefs-insert-buttons)
        (setq prefs (elim-avalue "prefs" (elim-avalue "value" args)))
        (mapc
         (lambda (pref)
           (apply 'widget-create 'tree-widget
                  :open       t
                  :node       (garak-ui-pref-to-node pref)
                  :inline     t
                  (garak-ui-pref-children pref)) ) prefs)
        (garak-ui-prefs-insert-buttons)
        (widget-setup) )) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file transfers
(defun garak-ui-ft-create-buffer (proc)
  (let ((old-buffer (elim-fetch-process-data proc :ft-buffer)) 
        (icons      (copy-sequence garak-icons))
        ft-buffer)
    ;; make sure we have a ui buffer
    (setq ft-buffer (or old-buffer (get-buffer "*garak file transfers*")))
    (when (not (garak-buffer-reusable proc ft-buffer))
      (setq ft-buffer (generate-new-buffer "*garak file transfers*")))
    (when (not (eq old-buffer ft-buffer))
      (with-current-buffer ft-buffer
        (elim-init-ui-buffer)
        (garak-init-local-storage)
        (setq garak-elim-process proc)
        ;; initialise tree-widget support in this buffer
        (tree-widget-set-theme)
        ;; add our icons into the whatever tree-widget theme we got:
        (setq icons (nconc icons (aref tree-widget--theme 3)))
        (aset tree-widget--theme 3 icons)
        ;; done with icons
        (use-local-map widget-keymap)
        (elim-store-process-data proc :ft-buffer ft-buffer)))
    ft-buffer))

(defun garak-transfer-status (proc name id status args)
  (let ((uid (elim-avalue "xfer-uid" args)) buf)
    (with-current-buffer (setq buf (garak-ui-ft-create-buffer proc))
      (when (eq (garak-ui-ft-update-display proc args) :created)
        (let ((display-buffer-reuse-frames t)) 
          (display-buffer buf)) )) ))

(defun garak-transfer-percent (proc name id status args)
  ;;(when nil
  (let ((uid      (elim-avalue "xfer-uid"     args)) 
        (progress (elim-avalue "xfer-percent" args)) 
        (buf      (elim-fetch-process-data proc :ft-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (garak-ui-ft-update-progress uid progress))) ))
;;)

(defun garak-ui-ft-locate (uid)
  (let ((start  nil)
        (end    nil)
        (pos   (point-min)) 
        (sprop 'garak-xfer-start)
        (eprop 'garak-xfer-end  ))
    (save-excursion
      (when (equal (get-text-property pos sprop) uid)
        (setq start pos))
      (while (and (not start) (< pos (point-max))
                  (setq pos (next-single-char-property-change pos sprop nil)))
        (when (equal (get-text-property pos sprop) uid) (setq start pos)))
      (while (and (not end) (< pos (point-max))
                  (setq pos (next-single-char-property-change pos eprop nil)))
        (when (equal (get-text-property pos eprop) uid) (setq end pos))))
    (when (and start end) (cons start end)) ))

(defun garak-ui-ft-update-progress (uid progress)
  (let (ft-region)
    (setq progress   (if (numberp progress) progress 0.0)
        ;;progress   (make-string (round (/ progress 10.0)) ?*)
          progress   (propertize (format "%05.1f" progress) 'ft-progress uid))
    (when (setq ft-region (garak-ui-ft-locate uid))
      (let ((start (car ft-region))
            (end   (cdr ft-region))
            (pos    nil)
            (icon  (tree-widget-find-image ":available"))
            (label (elim-avalue ":available" garak-icon-tags))
            (inhibit-redisplay   t))
          (setq pos (next-single-char-property-change start 'ft-progress))
          (save-excursion
            (delete-region pos (+ 5 pos))
            (goto-char pos)
            (insert progress)
            (when (and (display-images-p) ft-icon)
              (setq pos  (next-single-char-property-change start 'ft-state)
                    pos2 (next-single-char-property-change pos   'ft-state))
              (when (and pos pos2 (< pos pos2))
                (goto-char pos)
                (when (not (looking-at (regexp-quote label)))
                  (delete-region pos pos2)
                  (insert (propertize label 
                                      'display  icon 
                                      'ft-state uid ))) ))) t)) ))

(defun garak-ui-ft-update-display (proc xfer)
  (let ((uid (elim-avalue "xfer-uid" xfer)) 
        (inhibit-read-only          t)
        (inhibit-point-motion-hooks t)
        (inhibit-modification-hooks t)
        ft-region ft-display other-user icon-name ft-state  s-label complete
        file      size       p-icon     p-label   direction pos     pos2)
    (setq progress   (or (elim-avalue "xfer-progress" xfer) 0.0)
          complete   (eql progress 100.0)
        ;;progress   (make-string (round (/ progress 10.0)) ?*)
          progress   (propertize (format "%05.1f" progress) 'ft-progress uid)
          ft-state   (elim-avalue "xfer-status" xfer)
          ft-icon    (cond (complete                     ":on"         )
                           ((eq :not-started   ft-state) ":away"       )
                           ((eq :accepted      ft-state) ":available"  )
                           ((eq :started       ft-state) ":available"  )
                           ((eq :done          ft-state) ":on"         )
                           ((eq :cancel-local  ft-state) ":off"        )
                           ((eq :cancel-remote ft-state) ":unavailable")
                           (t                            ":offline"    ))
          s-label    (elim-avalue ft-icon garak-icon-tags)
          ft-icon    (tree-widget-find-image ft-icon))
    (when (and (display-images-p) ft-icon)
      ;;(message "propertising label %S <-\n%S" s-label ft-icon)
      (setq s-label (propertize s-label 'display ft-icon 'ft-state uid)))
    (if (setq ft-region (garak-ui-ft-locate uid))
        ;; update existing xfer
        (let ((start (car ft-region)) 
              (end   (cdr ft-region)) 
              (inhibit-redisplay   t))
          ;;(message "updating existing widget %S [%s]" uid progress)
          (setq pos (next-single-char-property-change start 'ft-progress))
          (delete-region pos (+ 5 pos))
          ;;(replace-regexp ".\\{5\\}" progress nil pos (+ 5 pos))
          (save-excursion
            (goto-char pos)
            (insert progress)
            (when ft-icon
              (setq pos  (next-single-char-property-change start 'ft-state)
                    pos2 (next-single-char-property-change pos   'ft-state))
              (when (and pos pos2 (< pos pos2))
                (delete-region pos pos2)
                ;;(message "updating icon %s" s-label)
                (goto-char pos)
                (insert s-label)) )) :updated)
      ;;(message "creating new widget %S" uid)
      (setq icon-name  (format ":%s" (elim-avalue "im-protocol" xfer))
            file       (elim-avalue "xfer-local-file"  xfer)
            size       (elim-avalue "xfer-size"        xfer)
            other-user (elim-avalue "xfer-remote-user" xfer)
            direction  (elim-avalue "xfer-type"        xfer)
            direction  (if (eq :receive direction) "->" "<-")
            p-icon     (tree-widget-find-image icon-name)
            p-label    (elim-avalue icon-name garak-icon-tags)
            p-label    (format "[%s]" (or p-label "----")))
      (when (and (display-images-p) p-icon)
        (setq p-label (propertize p-label 'display p-icon)))
      (setq ft-display
            (format "%s%15s: %s %s %s [%5s%%] %d Bytes"
                    p-label other-user direction s-label file progress size))
      (save-excursion
        (end-of-buffer)
        (insert (propertize " "  'garak-xfer-start uid)
                ft-display
                (propertize "\n" 'garak-xfer-end   uid))) :created)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buddy list & account list ui
(defun garak-ui-create-widget-buffer (proc)
  (when (tree-widget-use-image-p) (garak-load-icons))
  (let ((blist   (elim-buddy-list proc))
        (icons   (copy-sequence garak-icons))
        (bbuffer (or (elim-fetch-process-data proc :blist-buffer) 
                     (get-buffer "*Garak*"))))
    (when (not (garak-buffer-reusable proc bbuffer))
      (setq bbuffer (generate-new-buffer "*Garak*")))
    (elim-store-process-data proc :blist-buffer bbuffer)
    (with-current-buffer bbuffer
      (elim-init-ui-buffer)
      (garak-init-local-storage)
      (setq garak-elim-process proc)
      ;; initialise tree-widget support in this buffer
      (tree-widget-set-theme)
      ;; add our icons into the whatever tree-widget theme
      ;; the user wanted as the default:
      (setq icons (nconc icons (aref tree-widget--theme 3)))
      (aset tree-widget--theme 3 icons)
      (add-hook 'tree-widget-before-create-icon-functions
                'garak-ui-node-setup-icon nil t)
      (setq elim-form-ui-args (list :process proc))
      (garak-insert-account-list)
      (mapc
       (lambda (N)
         (garak-insert-buddy-list-toplevel proc N)) blist)
      (use-local-map widget-keymap)
      (widget-setup))
    bbuffer))

(defun garak-menu-actions-to-choices (raw &optional cooked)
  (mapc 
   (lambda (entry)
     (cond ((listp (cdr entry))
            (setq cooked (cons (car entry) cooked)
                  cooked (nreverse 
                          (garak-menu-actions-to-choices (cdr entry) cooked))))
           (t (setq cooked (cons entry cooked))) )) raw)
  (nreverse cooked))

(defun garak-account-menu-response-handler (proc name id attr args &optional ev)
  (let (acct-data acct-uid acct-name choices menu value arglist call)
    (when (equal (elim-avalue "status" args) 0)
      (setq args      (elim-avalue "value"        args)
            acct-uid  (elim-avalue "account-uid"  args)
            acct-name (elim-avalue "account-name" args)
            menu      (elim-avalue "menu"         args)
            choices   (garak-menu-actions-to-choices menu)
            value     (when menu (widget-choose acct-name choices ev)))
      (when (and (numberp value) (not (zerop value)))
        (setq arglist (list "account-uid" acct-uid "menu-action" value)
              arglist (elim-simple-list-to-proto-alist arglist)
              call    (elim-daemon-call 'account-menu-action nil arglist))
        (elim-process-send proc call)) )))

(defun garak-buddy-menu-response-handler (proc name id attr args &optional ev)
  (let (bnode-data bnode-uid bnode-name choices menu value arglist call)
    (when (equal (elim-avalue "status" args) 0)
      (setq args       (elim-avalue "value"       args)
            bnode-uid  (elim-avalue "bnode-uid"   args)
            menu       (elim-avalue "menu"        args)
            bnode-data (elim-buddy-data proc bnode-uid)
            bnode-name (elim-avalue "bnode-name" bnode-data)
            choices    (garak-menu-actions-to-choices  menu)
            value      (when menu (widget-choose bnode-name choices ev)))
      (when (numberp value)
        (setq arglist (list "bnode-uid" bnode-uid "menu-action" value)
              arglist (elim-simple-list-to-proto-alist arglist)
              call    (elim-daemon-call 'buddy-menu-action nil arglist))
        (elim-process-send proc call) )) ))

(defun garak-buddy-list-node-command (&optional widget child event &rest stuff)
  (let (value op buid proc buddy auid bname menu-cb)
    (setq proc  garak-elim-process
          value (widget-value widget)
          op    (car value)
          buid  (cdr value)
          buddy (elim-buddy-data proc buid)
          bname (elim-avalue "bnode-name"  buddy)
          auid  (elim-avalue "account-uid" buddy))
    (elim-debug "%S" (list op auid buid))
    (cond ((eq op :del ) (elim-remove-buddy proc nil  buid))
          ((eq op :join) (elim-join-chat    proc auid buid))
          ((eq op :info) (elim-buddy-info   proc auid buid))
          ((eq op :xfer) (elim-send-file    proc auid buid))
          ((eq op :priv) (elim-toggle-user-blocked proc buid auid))
          ((eq op :menu)
           (lexical-let ((m-event event))
             (setq menu-cb 
                   (lambda (proc name id attr args) 
                     (garak-buddy-menu-response-handler 
                      proc name id attr args m-event)))
             (elim-buddy-menu proc buid menu-cb) ))
          ((eq op :msg ) (elim-message proc auid bname
                                       (read-string (format "IM %s> " bname))))
          (t (elim-debug "UI Buddy Operation `%S' not implemented" op))) ))

(defun garak-maybe-remove-account (account)
  (let ((aname (elim-avalue :name account)))
    (when (y-or-n-p (format "remove %s? " aname))
      (garak-cmd-remove-account (car account)) )))

(defun garak-account-list-node-command (&optional widget child event &rest x)
  (let (value op proc acct auid ccb menu-cb)
    (setq proc  garak-elim-process
          value (widget-value widget)
          ccb   'garak-account-options-ui-cb
          op    (car value)
          auid  (cdr value)
          acct  (elim-account-data proc auid))
    (cond ((eq op :login ) (elim-connect         proc auid))
          ((eq op :logout) (elim-disconnect      proc auid))
          ((eq op :config) (elim-account-options proc auid ccb))
          ((eq op :remove) (garak-maybe-remove-account acct))
          ((eq op :menu  )
           (lexical-let ((m-event event))
             (setq menu-cb
                   (lambda (proc name id attr args)
                     (garak-account-menu-response-handler proc name id
                                                          attr args m-event)))
             (elim-account-menu proc auid menu-cb) ))
          (t (elim-debug "UI Account Operation `%S' not implemented" value))) ))

(defun garak-buddy-list-node-widget (proc bnode)
  (let (kids uid menu type name mtail plabel blabel auid aicon alias cname)
    (setq uid   (elim-avalue "bnode-uid"     bnode)
          alias (elim-avalue "bnode-alias"   bnode)
          cname (elim-avalue "contact-alias" bnode)
          name  (elim-avalue "bnode-name"    bnode)
          type  (elim-avalue "bnode-type"    bnode)
          auid  (elim-avalue "account-uid"   bnode)
          name  (if (< 0 (length cname)) cname name)
          name  (if (< 0 (length alias)) alias name)
          mtail (list (garak-choice-item "--single-line")
                      (garak-choice-item "Remove" (cons :del uid))
                      (garak-choice-item "--single-line"))
          kids  (mapcar
                 (lambda (N) (garak-buddy-list-node-widget proc N))
                 (delq nil (elim-buddy-children proc uid)))
          menu  (cond ((eq type :chat-node )
                       (list (garak-choice-item "Join" (cons :join uid))))
                      ((eq type :buddy-node)
                       (list 
                        (garak-choice-item "Get Info"  (cons :info uid))
                        (garak-choice-item "Send IM"   (cons :msg  uid))
                        (garak-choice-item "Send File" (cons :xfer uid))
                        (garak-choice-item "[Un]Block" (cons :priv uid)))) )
          menu  (cons (garak-choice-item "--no-line") ;;'(noop))
                      (nconc menu mtail)))
    (when (memq type '(:chat-node :buddy-node))
      (setcdr (last mtail)
              (list (garak-choice-item "Extended Menu -->" (cons :menu uid))) ))
    ;; pick an account icon if this bnode has an account and we want icons
    (when (and auid (tree-widget-use-image-p))
        (let (proto iname adata)
          (setq adata (elim-account-data proc auid)
                proto (elim-avalue :proto adata)
                iname (format ":%s" proto)
                aicon (tree-widget-find-image iname))
          ;;(message "bnode account icon: %s" iname)
          ))
    ;; set up the label, with icon if we got one above:
    (setq blabel (if aicon (concat (propertize " " 'display aicon) name) name))
    (if kids
        (apply 'widget-convert 'tree-widget
               :open       t
               :buddy      uid
               :value      uid
               :garak-type :bnode
               :expander   'garak-buddy-list-node-children
               :node       (apply 'widget-convert 'menu-choice
                                  :format    "%[%t%]\n"
                                  :tag        blabel
                                  :mouse-down-action 
                                    'garak-menu-choice-mouse-down-action
                                  :value      '(noop)
                                  :value-get 'widget-value-value-get
                                  :inline     t
                                  :notify    'garak-buddy-list-node-command
                                  menu)
               kids)
      (apply 'widget-convert
             'menu-choice
             :format            "%[%t%]\n"
             :tag                blabel
             :value             '(noop)
             :buddy              uid
             :garak-type        :bnode
             :value-get         'widget-value-value-get
             :mouse-down-action 'garak-menu-choice-mouse-down-action
             :inline             t
             :notify            'garak-buddy-list-node-command
             menu                )) ))

(defun garak-buddy-list-skip (proc bnode)
  (cond
   ((and garak-hide-offline-buddies
         (eq (elim-avalue "contact-online-buddies" bnode) 0)) 
    nil)
   ((eq (elim-avalue "contact-size" bnode) 1)
    (or (elim-buddy proc (elim-avalue "contact-main-child-uid" bnode)) bnode))
   (t bnode)) )

;(defun garak-buddy-list-skip (proc bnode)
;  (if (equal (elim-avalue "contact-size" bnode) 1)
;      (or (elim-buddy proc (elim-avalue "contact-main-child-uid" bnode)) bnode)
;    bnode))

(defun garak-account-list-node-widget (process account)
  (let (adata uid proto iname icon label aname)
    (setq uid   (car account)
          adata (cdr account)
          aname (elim-avalue :name  adata)
          proto (elim-avalue :proto adata)
          iname (format ":%s" proto)
          alt   (format "[%-4s] " 
                        (or (elim-avalue iname garak-icon-tags) " ?? "))
          icon  (or (tree-widget-find-image iname)
                    (tree-widget-find-image ":prpl-generic")))
          
    ;;(message "garak-account-list-node-widget: %S" account)
    (setq label (if (and icon (tree-widget-use-image-p))
                    (concat (propertize alt 'display icon) aname)
                  (concat alt aname)))
    (widget-convert 'menu-choice
                    :format            "%[%t%]\n"
                    :garak-type        :account
                    :tag                label
                    :im-protocol        proto
                    :value             '(noop)
                    :account            uid
                    :mouse-down-action 'garak-menu-choice-mouse-down-action
                    :value-get         'widget-value-value-get
                    :inline             t
                    :notify            'garak-account-list-node-command
                    (garak-choice-item "Log In"        (cons :login  uid))
                    (garak-choice-item "Log Out"       (cons :logout uid))
                    (garak-choice-item "--single-line")
                    (garak-choice-item "Remove"        (cons :remove uid))
                    (garak-choice-item "--single-line")
                    (garak-choice-item "Configure"     (cons :config uid))
                    (garak-choice-item "Extended Menu" (cons :menu   uid))) ))

(defun garak-account-list-node-children (&optional widget)
  (mapcar
   (lambda (N)
     (garak-account-list-node-widget garak-elim-process N))
   (elim-account-alist garak-elim-process)))

(defun garak-insert-account-list ()
  (apply 'widget-create 'tree-widget
         :open        t
         :expander   'garak-account-list-node-children
         :garak-type :accounts
         :tag        "Accounts"
         (garak-account-list-node-children)))

(defun garak-buddy-list-node-children (widget)
  (let ((uid (widget-get widget :buddy)) children process dummy kids)
    ;;(message "Updating children for node %S" uid)
    (setq process  garak-elim-process
          children (elim-buddy-children process uid))
    ;;(elim-debug "(garak-buddy-list-node-children %S) -> %S" (car widget) uid)
    (setq kids (mapcar
                (lambda (N)
                  (let ((bnode (garak-buddy-list-skip process N)))
                    (when bnode
                      (garak-buddy-list-node-widget process bnode))))
                children))
    (delq nil kids)))

(defun garak-insert-buddy-list-top (proc bnode)
  (let ((uid (elim-avalue "bnode-uid" bnode)) menu name kids alias)
    (setq ;remove (garak-choice-item "Delete All" (cons :del uid))
          name   (elim-avalue "bnode-name"  bnode)
          alias  (elim-avalue "bnode-alias" bnode)
          name   (if (> (length alias) 0) alias name)
          kids   (mapcar
                  (lambda (N)
                    (let ((node (garak-buddy-list-skip proc N)))
                      (when node (garak-buddy-list-node-widget proc node))))
                  (elim-buddy-children proc (elim-avalue "bnode-uid" bnode)))
          kids   (delq nil kids))
    (if kids
        (apply 'widget-create
               'tree-widget
               :open       t
               :tag        name
               :garak-type :bnode
               :help-echo nil
               :buddy      uid
               :value      uid
               :expander  'garak-buddy-list-node-children
               kids )
      (setq menu (list (garak-choice-item ""       (cons :noop uid))
                       (garak-choice-item "Remove" (cons :del  uid))))
      (apply 'widget-create
             'tree-widget
             :open       t
             :tag        name
             :garak-type :bnode
             :help-echo  nil
             :buddy      uid
             :value      uid
             :expander  'garak-buddy-list-node-children
             :node      (apply 'widget-convert 'menu-choice
                               :format    "%[%t%]\n"
                               :tag        name
                               :value      (cons 'noop uid)
                               :value-get 'widget-value-value-get
                               :inline     t
                               :notify    'garak-buddy-list-node-command
                               menu) nil)) ))

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
    ;;(elim-debug "GARAK-TREE-WIDGET-APPLY %S->%S %S"
    ;;            (car (garak-tree-widget-real-target widget))
    ;;            (garak-tree-widget-get widget prop)
    ;;            args)
    (apply 'widget-apply (garak-tree-widget-real-target widget) prop args)))

(defun garak-ui-find-node (uid type)
  (let ((last-point -1) (found nil) (widget nil)
        (inhibit-point-motion-hooks t)
        (inhibit-redisplay          t))
    (save-excursion
      (beginning-of-buffer)
      (while (and (not found) (< last-point (point)))
        (when (and (setq widget (widget-at))
                   (eql (garak-tree-widget-get (widget-at) type) uid))
          (setq found (cons (point) (car widget))))
        (setq last-point (point))
        (ignore-errors (widget-forward 1)) ))
    found))

;;!! update icon of visible node
(defun garak-update-buddy (proc name id status args)
  (let (buid buddy where-widget point widget icon-name icon buffer tag other)
    (setq buffer (elim-fetch-process-data proc :blist-buffer))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq buid  (elim-avalue    "bnode-uid" args )
              buddy (elim-buddy-data       proc buid )
              other (garak-buddy-list-skip proc buddy))
        ;; if the bnode is not in the "ignored" class, buddy will eq other:
        (when (eq buddy other)
          (setq buid         (elim-avalue "bnode-uid"   args)
                where-widget (garak-ui-find-node buid :buddy))
          ;; if where-widget is nil, the bnode is not displayed: find the
          ;; parent node and clear its cache, and refresh it if it's open:
          (if (not where-widget)
              (let ((puid (garak-buddy-find-parent proc buid)) parent kfun kids)
                (if (setq where-widget (garak-ui-find-node puid :buddy))
                    (progn
                      (setq parent (elim-buddy-data proc puid)
                            point  (car where-widget)
                            widget (widget-at  point))
                      (when (and (memq (elim-avalue "bnode-type" parent)
                                       '(:group-node :contact-node))
                                 (garak-tree-widget-get widget :node))
                        (setq kids (garak-tree-widget-apply widget :expander))
                        (garak-tree-widget-set widget :args kids)
                        (when (garak-tree-widget-get widget :open)
                          (widget-apply widget :action)
                          (widget-apply widget :action)) ))
                  (save-excursion
                    (end-of-buffer)
                    (garak-insert-buddy-list-top proc
                                                 (elim-buddy-data proc puid)))))
            ;; the widget is currently visible. tweak it by hand
            ;; as this seems to be the only reliable way to refresh it:
            (setq point     (car where-widget)
                  end       (next-single-char-property-change point 'display)
                  widget    (cdr where-widget)
                  icon-name (garak-buddy-list-choose-icon widget buddy)
                  tag       (elim-avalue icon-name garak-icon-tags)
                  icon      (tree-widget-find-image icon-name))
            (let ((inhibit-read-only t) (old))
              (setq widget (widget-at point)
                    old    (widget-get widget :tag))
              (widget-put widget :tag tag)
              (if (and icon (tree-widget-use-image-p))
                  (put-text-property point end 'display icon)
                (when tag
                  (setq end (+ point (length old)))
                  ;;(message "REPLACE %S with %S in %S - %S"
                  ;;         old tag point end)
                  (save-excursion
                    (goto-char point)
                    (setq old (make-string (length old) ?.))
                    (when (search-forward-regexp old end t)
                      (replace-match tag nil t)) )) )) )) )) ))

(defalias 'garak-connection-progress 'garak-account-update)
(defun garak-account-update (proc name id status args)
  "This function handles updating the garak ui when the state of one of your
accounts changes. Typically this is as a result of elim-account-status-changed
elim-connection-state or elim-connection-progress, but any call can be handled as long as an \"account-uid\" entry is present in the ARGS alist."
  (let (buffer auid where-widget point end icon-name icon conn kids node tag)
    (setq buffer (elim-fetch-process-data proc :blist-buffer)
          status nil)
    (elim-debug "(garak-account-update.%S ...)" name)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq auid         (elim-avalue "account-uid" args)
              where-widget (garak-ui-find-node auid :account))

        ;; set the conn or status data (whichever is appropriate)
        (cond ((eq name 'elim-account-status-changed) (setq status args))
              ((eq name 'elim-connection-state      ) (setq conn   args))
              ((eq name 'elim-connection-progress   ) (setq conn   args)))

        ;; fetch any data we did not receive:
        (when (not conn  ) (setq conn   (elim-account-connection proc auid)))
        (when (not status) (setq status (elim-account-status     proc auid)))

        ;; pick the most suitable status icon
        (if (eq name 'elim-exit)
            (setq icon-name ":offline")
          (setq icon-name (garak-account-list-choose-icon conn status)))
        ;;(message "CHOSE ICON: %S" icon-name)
        ;; widget not found or removing an account => refresh the parent node.
        ;; otherwise                               => update node icon
        (if (or (eq 'remove-account name) (not where-widget))
            ;; refreshing parent node:
            (when (setq where-widget (garak-ui-find-node :accounts :garak-type)
                        point        (car where-widget))
              (setq node (widget-at point)
                    kids (garak-tree-widget-apply node :expander))
              (garak-tree-widget-set node :args kids)
              (when (garak-tree-widget-get node :open)
                (widget-apply node :action)
                (widget-apply node :action)))
          ;; updating node icon:
          (setq point (car where-widget)
                end   (next-single-char-property-change point 'display)
                tag   (elim-avalue icon-name garak-icon-tags)
                icon  (tree-widget-find-image icon-name))
          (let ((inhibit-read-only t) old)
            (setq widget (widget-at point)
                  old    (widget-get widget :tag))
            (widget-put widget :tag tag)
            (if (and icon (tree-widget-use-image-p))
                (put-text-property point end 'display icon)
              (when tag
                (setq end (+ (length old) point))
                (save-excursion
                  (goto-char point)
                  (setq old (make-string (length old) ?.))
                  (when (search-forward-regexp old end t)
                    (replace-match tag nil t)) )) )) )) )))

(defun garak-delete-buddy (proc name id status args)
  (let ((inhibit-read-only t) buid puid where-widget point widget buffer dummy)
    (setq buffer (elim-fetch-process-data proc :blist-buffer))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq buid (elim-avalue "bnode-uid"    args)
              puid (elim-avalue "bnode-parent" args))
        (message "garak-delete-buddy: %S" puid)
        (if puid
            (let (parent kfun kids)
              ;; if the parent is skippable, find a non-skippable ancestor:
              ;; we can't use normal buddy methods because it has already
              ;; been deleted from elim's cache by now: the parent should still
              ;; be alive though as children are reaped before ancestors:
              (setq parent (elim-buddy-data proc puid))
              (when (equal (elim-avalue "contact-size" parent) 1)
                (setq puid   (garak-buddy-find-parent proc puid)
                      parent (elim-buddy-data proc puid)))
              ;; ok: by now we should have a live ancestor: find its node:
              (when (setq where-widget (garak-ui-find-node puid :buddy))
                (setq point  (car where-widget)
                      widget (widget-at  point))
                (when (and (memq (elim-avalue "bnode-type" parent)
                                 '(:group-node :contact-node))
                           (garak-tree-widget-get widget :node))
                  (setq kids (garak-tree-widget-apply widget :expander))
                  (garak-tree-widget-set widget :args kids)
                  (when (garak-tree-widget-get widget :open)
                    (widget-apply widget :action)
                    (widget-apply widget :action))) ))
          ;; need to delete a top-level node:
          (when (and (setq where-widget (garak-ui-find-node buid :buddy))
                     (setq point  (car where-widget)
                           widget (widget-at point)
                           widget (garak-tree-widget-real-target widget)))
            (widget-children-value-delete widget)
            (message "point AFTER deletion: %S" (point))
            (when (eq (car-safe (get-text-property (1- point) 'display)) 'space)
              (delete-char -1)) )) )) ))

(defun garak-buddy-list-choose-icon (widget buddy)
  (let ((class (when (consp widget) (car widget) widget)) type blocked)
    (setq type    (elim-avalue "bnode-type" buddy)
          allowed (elim-avalue "allowed"    buddy))
    (cond ((eq class 'tree-widget-empty-icon) ":invisible")
          ((eq type :chat-node              ) ":chat"     )
          ((eq type :group-node             ) ":group"    )
          ((eq type :contact-node           )
           (setq online (elim-avalue "contact-online-buddies" buddy))
           (if (and online (< 0 online)) ":person" ":off"))
          ((eq type :buddy-node             )
           (if (not allowed) ":blocked"
             (symbol-name (elim-avalue "status-type" buddy))) )) ))

(defun garak-account-list-choose-icon (conn-data status-data)
  (let ((status (elim-avalue "status-type" status-data))
        (conn   (elim-avalue "state"       conn-data  ))
        (online (elim-avalue "connected"   status-data)) icon)
    (setq icon (cond ((eq   conn :connecting   ) ":connecting"   )
                     ((eq   conn :disconnected ) ":off"          )
                     ((null conn               ) ":off"          )
                     ((eq status :offline      ) ":off"          )
                     ((eq status :available    ) ":on"           )
                     ((eq status :unavailable  ) ":unavailable"  )
                     ((eq status :invisible    ) ":invisible"    )
                     ((eq status :away         ) ":away"         )
                     ((eq status :extended-away) ":extended-away")
                     ((eq status :mobile       ) ":away"         )
                     ((eq status :tune         ) ":away"         )
                     ((eq conn   :connected    ) ":on"           )
                     (online                     ":on"           )
                     (t                          ":off"          )))
    ;;(message "%s -> %s" (elim-avalue "account-name" status-data) icon)
    icon))

(defun garak-ui-node-setup-icon (wicon)
  (let ((class   (car wicon))
        (process garak-elim-process)
        buddy buid account auid status icon tag online gtype conn)
    (setq gtype (garak-tree-widget-get wicon :garak-type))
    ;; choose an appropriate icon, depending on the node type:
    (cond ((eq gtype :bnode)
           (when (and (setq buid  (garak-tree-widget-get wicon :buddy))
                      (setq buddy (elim-buddy-data process buid )))
             (setq icon (garak-buddy-list-choose-icon wicon buddy)) ))
          ((eq gtype :account)
           (when (setq auid (garak-tree-widget-get wicon :account))
             (setq status (elim-account-status     process auid)
                   conn   (elim-account-connection process auid))
             (setq icon (garak-account-list-choose-icon conn status)) ))
          ((eq gtype :accounts) (setq icon ":garak")))
    ;; set the icon, if we have an image in our set:
    (when (and icon (assoc icon garak-icons))
      (when (tree-widget-use-image-p)
        (widget-put wicon :glyph-name icon))
      (when (setq tag (elim-avalue icon garak-icon-tags))
        (widget-put wicon :tag tag)) ) ))

(defun garak-request-add-buddy (proc name id status args)
  (let (buf buf-name user-name account-name group)
    (setq user-name    (elim-avalue "user-name"    args)
          account-name (elim-avalue "account-name" args)
          group        (elim-avalue "group"        args)
          buf-name     (format "*Add Buddy: %s*" user-name)
          buf          (generate-new-buffer buf-name))
    (with-current-buffer buf
      (elim-init-ui-buffer)
      (garak-init-local-storage)
      (setq garak-elim-process proc
            garak-account-uid  (elim-avalue "account-uid" args))
      (widget-insert (format "Add Buddy to %s\n" account-name))
      (elim-request-field-string "user-name" `(("label" . "User Name: ")
                                               ("value" . ,user-name   )))
      (elim-request-field-string "group"     `(("label" . "Group: ")
                                               ("value" . ,group   )))
      (elim-form-widget-create 'push-button
                               nil
                               :format (format "[%%[%s%%]]" "Cancel")
                               :notify 'garak-request-nok)
      (widget-insert " ")
      (elim-form-widget-create 'push-button
                               nil
                               :format (format "[%%[%s%%]]" "Add")
                               :notify 'garak-request-ok)
      (use-local-map widget-keymap)
      (widget-setup)
      (beginning-of-buffer)
      (widget-forward 1))
    (display-buffer buf)))
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
    ;; (message "(elim-add-account PROC %S %S %S %S)" user proto pass nil)
    (elim-add-account elim user proto pass options)
    (format "/add-account %s" args) ))

(defun garak-cmd-add-buddy (args)
  (let (items account proto buddy errval adata group)
    (setq account
          (garak-cmd-strip-account-arg garak-elim-process items args adata)
          buddy (car  items)
          group (cadr items))
    (condition-case errval
        (progn
          (elim-add-buddy garak-elim-process account buddy group)
          (format "/add-buddy %s %s" buddy group))
        (error "Could not add buddy: %S" errval)) ))

(defun garak-cmd-send-file (args)
  (let (items account proto buddy adata file)
    (setq account
          (garak-cmd-strip-account-arg garak-elim-process items args adata)
          buddy (car  items)
          file  (cadr items))
    (elim-send-file garak-elim-process account buddy file)
    (format "/send %s" args) ))

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

(defun garak-cmd-status (args)
  (let (items type id message mregex tsym)
    (setq items   (split-string args)
          type    (car  items))
    (if type
        (progn
          (if (and (eq (aref type 0) ?:) (setq tsym (intern-soft type)))
              (setq type   (elim-pack-enum :status-primitive tsym)
                    id     (cadr items)
                    mregex "^\\s-*\\S-+\\s-+\\S-+\\s-+\\(.*?\\)\\s-*$")
            (setq id     type
                  type   (elim-standard-status-type id)
                  mregex "^\\s-*\\S-+\\s-+\\(.*?\\)\\s-*$"))
          (when (string-match mregex args)
            (setq message (match-string 1 args)))
          (elim-set-status garak-elim-process id message type)
          (format "/status %s" args))
      (format "/status %s: should be /status :TYPE NAME MESSAGE..." args)) ))

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

(defun garak-cmd-prefs (args)
  (garak-prefs) args)

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
    (prefs        . garak-cmd-prefs            )
    (quit         . garak-cmd-quit             )
    (register     . garak-cmd-register         )
    (remove-acct  . garak-cmd-remove-account   )
    (remove-buddy . garak-cmd-remove-buddy     )
    (send-file    . garak-cmd-send-file        )
    (status       . garak-cmd-status           )
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
   ((string-match "\\(?:^\\|/\\)prefs\\>"               cmd) 'prefs       )
   ((string-match "\\(?:^\\|/\\)\\(?:priv\\)?msg\\>"    cmd) 'msg         )
   ((string-match "\\(?:^\\|/\\)register\\>"            cmd) 'register    )
   ((string-match "\\(?:^\\|/\\)remove.account\\>"      cmd) 'remove-acct )
   ((string-match "\\(?:^\\|/\\)remove.buddy\\>"        cmd) 'remove-buddy)
   ((string-match "\\(?:^\\|/\\)send\\>"                cmd) 'send-file   )
   ((string-match "\\(?:^\\|/\\)status\\>"              cmd) 'status      )
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
     "/msg"            "/prefs"        "/quit"              "/register"
     "/remove-account" "/remove-buddy" "/send"              "/status" ))

(defvar garak-command-completers
  '((add-account . garak-comp-add-account)
    (add-buddy   . garak-comp-add-buddy  )
    (msg         . garak-comp-msg        )
    (connect     . garak-comp-account    )
    (config-acct . garak-comp-account    )
    (disconnect  . garak-comp-account    )
    (register    . garak-comp-account    )
    (remove-acct . garak-comp-account    )
    (send-file   . garak-comp-account    )
    (status      . garak-comp-status     )
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

(defun garak-comp-add-buddy (prefix &optional protocol)
  (let (acct args available)
    (setq args      (split-string prefix split-string-default-separators nil)
          available (mapcar (lambda (A) (cdr (assq :name (cdr A))))
                            (elim-account-alist garak-elim-process))
          acct      (or (nth 1 args) ""))
    (when (and (= (length args) 2) (not (member acct available)))
      (all-completions acct available)) ))

(defun garak-comp-account (prefix &optional protocol)
  (garak-comp-add-buddy prefix))

(defun garak-comp-msg (prefix &optional protocol)
  (garak-comp-add-buddy prefix))

(defun garak-comp-help (prefix &optional protocol)
  (let (cmd args)
    (setq args (split-string prefix split-string-default-separators nil)
          cmd  (or (nth 1 args) ""))
    (when (and (= (length args) 2) (not (member cmd garak-commands)))
      (all-completions cmd garak-commands)) ))

(defun garak-comp-join (prefix &optional protocol))

(defvar garak-standard-status-names (mapcar 'car elim-standard-status-types))
(defvar garak-standard-status-types
  (mapcar (lambda (x) (symbol-name (cdr x))) elim-standard-status-types))
(defvar garak-standard-status-things
  (nconc (copy-sequence garak-standard-status-names)
         garak-standard-status-types))

(defun garak-comp-status (prefix &rest ignore)
  "Complete /status commands of the form:\n
  /status :type-symbol label ... OR
  /status standard-label ..."
  (let (args type )
    (setq args (split-string prefix split-string-default-separators nil)
          type (or (nth 1 args) "")
          name (or (nth 2 args) ""))
    (message "ARGS: %S" args)
    (cond ((and (eq  (length args) 3)
                (not (member name garak-standard-status-names))
                (not (member type garak-standard-status-names)))
           (all-completions name garak-standard-status-names))
          ((and (eq  (length args) 2)
                (not (member type garak-standard-status-things)))
           (message "type: %S" type)
           (all-completions type garak-standard-status-things))) ))

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
(defvar garak-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") 'garak-toggle-offline-buddies)
    (define-key map (kbd "C-c C-c") 'garak-deactivate)
    (define-key map (kbd "C-c C-j") 'garak-activate)
    (define-key map (kbd "C-c C-g") 'garak-gui)
    map))

(define-derived-mode garak-mode lui-mode "Garak"
  "An IM mode based on elim"
  :group 'garak
  (setq lui-input-function 'garak-input-handler)
  (set (make-local-variable 'lui-possible-completions-function)
       'garak-complete)
  (lui-set-prompt (concat (propertize "garak>" 'face 'mode-line) " ")))

(defun garak-toggle-offline-buddies ()
  (interactive)
  (setq garak-hide-offline-buddies (not garak-hide-offline-buddies))
  (garak-gui))

(defun garak-deactivate ()
  (interactive)
  (cond (garak-conv-uid    (garak-cmd-leave        nil))
        (garak-account-uid (garak-cmd-disconnect   nil))
        (t                 (garak-cmd-status "offline")) ))

(defun garak-activate ()
  (interactive)
  (cond (garak-conv-uid     (message "Cannot reactivate conversations"))
        (garak-account-name (garak-cmd-connect garak-account-name))
        (t                  (garak-cmd-status "available")
                            (garak-cmd-status "available")) ))

(defun garak-gui ()
  "Create and display the garak buddy and account list widget buffer"
  (interactive)
  (let ((gui (garak-ui-create-widget-buffer garak-elim-process))
        (display-buffer-reuse-frames t))
    (display-buffer gui)))

(defun garak-prefs ()
  "Create and display the garak preferences buffer"
  (interactive)
  (let ((gui (garak-ui-create-prefs-buffer garak-elim-process))
        (display-buffer-reuse-frames t))
    (display-buffer gui)))

(defun garak ()
  (interactive)
  (let ((buffer (get-buffer "*garak*")) proc)
    (if (garak-buffer-reusable nil buffer)
        (switch-to-buffer buffer)
      (switch-to-buffer (setq buffer (generate-new-buffer "*garak*")))
      (garak-mode)
      (garak-init-local-storage))
    (setq lui-input-function 'garak-input-handler)
    (lui-insert "starting elim" t)
    (setq proc
          (elim-start "(elim . garak)" nil garak-callbacks))
    (with-current-buffer buffer (setq garak-elim-process proc))
    (elim-store-process-data garak-elim-process :cli-buffer buffer)
    (let ((display-buffer-reuse-frames t))
      (display-buffer (garak-ui-create-widget-buffer garak-elim-process)))
    (end-of-buffer)))

(provide 'garak)