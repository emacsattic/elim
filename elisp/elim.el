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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables & global state functions:
(defcustom elim-executable "elim-client" 
  "Location of the elim binary, or one that talks the same protocol"
  :group 'elim
  :type  '(file))

(defcustom elim-directory (expand-file-name "~/.emacs.d/elim")
  "elim client (libpurple) working directory. 
Buddy lists etc will be stored here"
  :group 'elim
  :type '(directory))

(defvar elim-enum-alist      nil
  "An alist of (:key (:label0 . value0) (:label1 . value1)) entries describing 
known libpurple enumerations.")
(defvar elim-enum-flag-types nil
  "A list of symbols which are keys in `elim-enum-alist' that are ORable
flags rather than simple enumerations.")
(defvar elim-initialising    nil
  "True when elim is still starting up.")

(defvar elim-call-handler-alist
  '(;; account ops
    (elim-account-notify-added  )
    (elim-account-status-changed)
    (elim-account-request-add   )
    (elim-account-request-auth  )
    ;; blist (buddy list) ops
    (elim-blist-create-node     )
    (elim-blist-remove-node     )
    (elim-blist-update-node     )
    ;; connection ops
    (elim-connection-state      )
    (elim-connection-progress   )
    (elim-disconnect-reason     )
    ;; network status
    (elim-network-up            )
    (elim-network-down          )
    ;; requests
    (elim-request-fields        )
    ;; conversation
    (elim-conv-create           )
    (elim-conv-destroy          )
    (elim-conv-write-chat       )
    (elim-conv-write-im         )
    (elim-conv-write-sys        )
    ;; chat
    (elim-chat-add-users        )
    (elim-chat-remove-users     )
    (elim-chat-rename-user      ) )
  "Alist of function call handlers. The car of a given element is the 
elim protocol function symbol. The cdr is the handler function, or nil
if the symbol to look for is the same as that of the protocol function.")

(defvar elim-resp-handler-alist
  '((add-account    . elim-response-filter-account-data)
    (add-buddy      . nil)
    (connect        . nil)
    (debug          . nil)
    (enumerations   . elim-enumerations-response)
    (disconnect     . nil)
    (init           . elim-init-response)
    (list-protocols . elim-list-protocols-response)
    (list-accounts  . elim-list-accounts-response )
    (message        . nil))
  "Alist of function response handlers. The car of a given element is the
elim protocol function symbol. The cdr is the handler function, or nil
if we do not intend to wait for the response (this is the usual case).")

(defvar elim-call-id          0 "State variable for generating call ids.")
(defvar elim-proto-buffer   nil "Buffer which the elim process filter uses.")
(defvar elim-process        nil "The elim daemon process.")
(defvar elim-conversations  nil "Alist of conversations keyed by uid.")
(defvar elim-accounts       nil "Alist of IM accounts keyed by UID.")

(defun elim-call-id ()
  (number-to-string (setq elim-call-id (1+ elim-call-id))))

(defun elim-command () 
  (format "%s 2>> %s/elim.$$.stderr" elim-executable elim-directory))
; (format "strace -f -o /tmp/elim.strace %s" elim-executable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protocol parsing and formatting:
(defun elim-unpack-enum (etype value)
  (let ((enum-vals (cdr (assq etype elim-enum-alist))) (rval nil))
    (if (memq etype elim-enum-flag-types)
        (mapc
         (lambda (v)
           (when (/= (logand (cdr v) value) 0) 
             (setq rval (cons (car v) rval)))) enum-vals)
      (setq rval (or (car (rassq value enum-vals)) value)))
    rval))

(defun elim-pack-enum (etype value)
  (let ((enum-vals (cdr (assq etype elim-enum-alist))) (rval 0))
    (if (memq etype elim-enum-flag-types)
        (mapc
         (lambda (v)
           (setq rval (logior rval (cdr (assq v enum-vals))))) 
         (if (listp value) value (list value)))
      (setq rval (or (cdr (assq value enum-vals)) value)))
    rval))

(defun elim-string-to-number (attr thing)
  (let (etype)
    (if (setq etype (intern-soft (cdr (assq 'type attr)))) 
        (elim-unpack-enum etype (string-to-number thing))
      (string-to-number thing))))

(defun elim-parse-proto-args (arg)
  "Take an elim protocol argument s-expression ARG and convert it into 
a straightforward elisp s-expression."
  (let ( (name   nil)
         (parsed nil)
         (type  (car  arg))
         (attr  (cadr arg))
         (value (cddr arg)) )
    (when attr (setq name (cdr (assq 'name attr))))
    (setq parsed
          (cond ((eq type 'string) (identity                   (car value)))
                ((eq type 'int   ) (elim-string-to-number attr (car value)))
                ((eq type 'float ) (string-to-number           (car value)))
                ((eq type 'bool  ) (/= 0 (string-to-number    (car value))))
                ((eq type 'data  ) (base64-decode-string       (car value)))
                ((eq type 'list  ) (mapcar  'elim-parse-proto-args   value))
                ((eq type 'alist ) (mapcar  'elim-parse-proto-args   value))
                (t                 (error  "Bad elim arg type : %S"   type)))) 
    (if name (cons name parsed) parsed)))

(defun elim-entity-to-string (match)
  (format "%c" (string-to-int (match-string 1 match))))

(defun elim-number-to-proto (number &optional attr)
  "Take a NUMBER and return an elim protocol sexp representing it"
  (when (not attr) (setq attr 'nil))
  (let ((val (number-to-string number)))
    (if (integerp number)
        (list 'int attr val)
      (if (string-match "^\\(-?[0-9]+\\)\\(?:.0\\)$" val)
          (list 'int attr (match-string 1 val))
        (list 'float attr val))) ))

(defun elim-atom-to-proto (x &optional n)
  "Return an elim protocol sexp representing X (a number, string or t or nil)."
  (let ((attr (if n (list (cons 'name n)) 'nil)))
    (message "(elim-atom-to-proto %S)" x)
    (cond ((stringp  x) (list               'string attr x))
          ((numberp  x) (elim-number-to-proto       x attr))
          ((booleanp x) (list  'bool  attr  (if x "1" "0")))
          ((symbolp  x) (list 'string attr (symbol-name x))) )))

(defun elim-atom-to-item (k v)
  "Take a number, t, nil or string V and prepare an elim protocol alist
item envelope with name K."
  (elim-atom-to-proto v k))

(defun elim-sexp-to-item (k v)
  "Take a sexp V already in elim protocol form and wrap it in an elim protocol 
alist item envelope with name K."
  (let ((item (copy-sequence v)))
    (nconc (list (car item) (list (cons 'name k))) (cddr item))))

(defun elim-simple-list-to-proto-alist (arg-list)
  "Take ARG-LIST of the form (string-key value string-key value ...)
and return an s-expression suitable for use as the argument in an elim
protocol function call or rsponse.\n
The values can be numbers, t, nil or strings."
  (let (k v rval)
    (while arg-list 
      (setq k        (format "%s" (car arg-list))
            v        (cadr arg-list)
            arg-list (cddr arg-list)
            rval     (cons (elim-atom-to-item k v) rval)))
    (nconc (list 'alist nil) (nreverse rval)) ))

(defun elim-daemon-call (name id args)
  "Take a call NAME (symbol), and ID (string or nil, usually nil) and 
a protocol arglist ARGS (eg as produced by `elim-simple-list-to-proto-alist'),
and return an s-expression suitable for making a call to an elim daemon."
  (let ( (cid (or id (elim-call-id))) )
    (if args
        (list 'function-call nil (list name (list (cons 'id cid))) args) 
      (list 'function-call nil (list name (list (cons 'id cid)))) )))

(defun elim-daemon-response (name id status value)
  (let (named-value type payload)
    (setq type        (car  value)
          payload     (cddr value)
          named-value (nconc (list type (list (cons 'name "value"))) payload))
    (message "PAYLOAD:\n%S" payload)
    (list 'function-response nil (list name (list (cons 'id id))) 
          (list 'alist nil
                (list 'int (list (cons 'name "status")) 
                      (number-to-string status))
                named-value)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun elim-debug (&rest args)
  (with-current-buffer (get-buffer-create "*elim-debug*")
    (beginning-of-buffer)
    (insert (apply 'format args) "\n\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; daemon i/o loop
(defun elim-handle-sexp (proc sexp)
  (elim-debug "RECEIVED: %S" sexp)
  (when (listp sexp)
    (let ((type (car   sexp))
          (name (caar (cddr sexp)))
          (attr (car  (cdar (cddr sexp))))
          (args (cadr (cddr sexp))) )
      (setq args (elim-parse-proto-args args))
      (elim-debug "received: %S.%S %S" type name args)
      (cond
       ((eq type 'function-call    ) (elim-handle-call proc name attr args))
       ((eq type 'function-response) (elim-handle-resp proc name attr args))
       (t (elim-debug "unknown protocol envelope: %S" type))) )) )

;; if we implement setting up response handlers for individual call instances
;; via elim-callback-alist, this is where we will fetch said handlers back:
(defun elim-set-resp-handler-by-id (proc id handler)
  (let ((store (elim-fetch-process-data proc :callbacks)) slot) 
    (if (setq slot (elim-avalue id store))
        (setcdr slot handler)
      (setq store (cons (cons id handler) store)))
    (elim-store-process-data proc :callbacks store)))

(defun elim-get-resp-handler-by-id (proc id)
  (let ((store (elim-fetch-process-data proc :callbacks)) slot)
    (when (setq slot (assoc id store))
      (setq store (assq-delete-all (car slot) store))
      (elim-store-process-data proc :callbacks store))
    (cdr slot)))

(defun elim-get-resp-handler-by-name (proc name)
  (cdr (assq name elim-resp-handler-alist)))

(defun elim-get-call-handler-by-name (proc name)
  (let ((slot (assq name elim-call-handler-alist)))
    (or (cdr slot) (car slot)) ))

(defun elim-handle-resp (proc name attr args)
  (let ( (id      (cdr (assq 'id attr))) 
         (handler  nil) )
    (setq handler (or (elim-get-resp-handler-by-id   proc id  ) 
                      (elim-get-resp-handler-by-name proc name)))
    ;; if told to use a handler but it hasn't been defined/loaded/implemented:
    (when (and handler (not (functionp handler)))
      (setq handler 'elim-default-response-handler))
    ;; finally, we must have a valid handler symbol to proceed:
    (when handler (funcall handler proc name id attr args)) ))

(defun elim-handle-call (proc name attr args)
  (let ( (handler (elim-get-call-handler-by-name proc name))
         (id      (cdr (assq 'id attr))) )
    (elim-debug "elim call handler: %S (%S)" handler (functionp handler))
    (if (functionp handler)
        (funcall handler proc name id 0 args)
      (elim-debug "elim-handle-call %s -> client" name)
      (elim-call-client-handler proc name id 0 args)) ))

(defun elim-input-filter (process data)
  (let ((pt nil) (sexp nil) (read-error nil) (sexp-list nil))
    (with-current-buffer (elim-fetch-process-data process :protocol-buffer)
      (setq pt    (point))
      (goto-char  (point-max))
      (insert      data)
      (goto-char   pt)
      (condition-case read-error
          (progn
            (while (setq sexp (read (current-buffer)))
              (setq sexp-list (cons sexp sexp-list))
              (delete-region pt (point))))
        (error
         (elim-debug "input filter: -- no more sexps remaining --")
       (goto-char pt)))
      (when sexp-list 
        ;;(setq sexp-list (nreverse sexp-list))
        (mapc (lambda (S) (elim-handle-sexp process S)) sexp-list)) )))

(defun elim-process-send (process sexp-value)
  (let ((print-level  nil) 
        (print-length nil)
        (sexp-string  nil))
    (setq sexp-string (prin1-to-string sexp-value))
    (process-send-string process sexp-string)
    (elim-debug "sent: %s" sexp-string)
    (accept-process-output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fetching and storing per-daemon data:
(defun elim-store-process-data (proc key value)
  (let ((elim-data (process-get proc :elim-data)))
    (setcdr (or (assq key elim-data)
                (car (setq elim-data (cons (cons key nil) elim-data)))) value)
    (process-put proc :elim-data elim-data)))

(defun elim-fetch-process-data (proc key)
  (cdr (assq key (process-get proc :elim-data))))

(defun elim-init-step (proc)
  (elim-store-process-data proc :initialised 
                           (1+ (elim-fetch-process-data proc :initialised))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; daemon response handlers:
(defun elim-unwrap-resp-args (status raw-args)
  (let (value)
    (when (not status) (setq status (elim-avalue "status" raw-args)))
    (cond ((equal status  0) (elim-avalue "value"   raw-args))
          ((integerp status) (elim-avalue "message" raw-args))
          (t raw-args)) ))

(defun elim-default-fail-handler (proc name id status message)
  (warn "%s<%s> failed (%S)" name id status value))

(defun elim-client-handler (proc name)
  (cdr (assq name (elim-fetch-process-data proc :client-ops))))

(defun elim-store-response-by-id (proc name id attr args)
  (let ((store (elim-fetch-process-data proc :response-by-id)))
    (setq store (cons (list id name attr args) store))
    (elim-store-process-data proc :response-by-id store)))

(defun elim-fetch-response-by-id (proc id)
  (let ((store (elim-fetch-process-data proc :response-by-id)) slot)
    (when (setq slot (assoc id store))
      (setq store (assq-delete-all (car slot) store))
      (elim-store-process-data proc :response-by-id store))
    slot))

(defun elim-call-client-handler (proc name id status args)
  (let ( (handler (elim-client-handler proc name)) )
    (message "retrieved client handler : %S" handler)
    (when (functionp handler)
      (elim-debug "calling client handler: %S" handler)
      (funcall handler proc name id status args)) ))

(defun elim-default-response-handler (proc name id attr args)
  (let ( (status (elim-avalue "status" args)) call-arg)
    (setq call-arg (elim-unwrap-resp-args status args))
    (if (equal status 0)
        (elim-debug "%s<%s> successful %S" name id call-arg)
      (elim-default-fail-handler proc name id status call-arg))
    (elim-call-client-handler proc name id status call-arg) ))

(defun elim-list-protocols-response (proc name id attr args)
  (let ((status (elim-avalue "status" args)) call-arg)
    (setq call-arg (elim-unwrap-resp-args status args))
    (if (not (equal status 0))
        (elim-default-fail-handler proc name id attr args)
      (elim-store-process-data proc :protocols call-arg) )
    (elim-call-client-handler proc name id status call-arg)
    (when elim-initialising (elim-init-step proc)) ))

(defun elim-enum-symbol (name)
  (intern (concat ":" (replace-regexp-in-string "_" "-" (downcase name)))))

(defun elim-enumerations-response (proc name id attr args)
  (when (equal (elim-avalue "status" args) 0)
    (let ((enum-alist (elim-avalue "value" args)) key entry)
      (mapc 
       (lambda (E) 
         (setq key   (intern (car E))
               entry (mapcar 
                      (lambda (F)
                        (cons (elim-enum-symbol (car F)) (cdr F))) (cdr E)))
         (setcdr (or (assq key elim-enum-alist)
                     (car (setq elim-enum-alist 
                                (cons (cons key nil) elim-enum-alist)))) entry))
       enum-alist))
    (mapc (lambda (E) 
            (when (string-match "-flags$" (symbol-name (car E))) 
              (add-to-list 'elim-enum-flag-types (car E)))) elim-enum-alist))
  (when elim-initialising (elim-init-step proc)) )

(defun elim-response-filter-account-data (proc name id attr args)
  (let (uid proto name store adata (val (elim-avalue "value" args)))
    (when (setq uid (elim-avalue "account-uid" val))
      (setq store (elim-fetch-process-data proc :accounts))
      (when (not (assoc uid store))
        (setq name  (elim-avalue "account-name" val)
              proto (elim-avalue "im-protocol"  val)
              adata (cons uid (list (cons :name  name ) 
                                    (cons :proto proto)))
              store (cons adata store))
        (elim-store-process-data proc :accounts store)))
      (or adata uid)))

(defun elim-list-accounts-response (proc name id attr args)
  (when (equal (elim-avalue "status" args) 0)
    (let (acl uid data name proto conn (accts (elim-avalue "value" args)))
      (mapc
       (lambda (acct) 
         (setq data  (cdr   acct)
               uid   (elim-avalue "account-uid"  data)
               name  (elim-avalue "account-name" data)
               proto (elim-avalue "im-protocol"  data)
               acl   (cons (cons uid (list (cons :name  name )
                                           (cons :proto proto))) acl))) accts)
      (elim-store-process-data proc :accounts acl)))
  (when elim-initialising (elim-init-step proc)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; daemon-to-client call handlers:

(defun elim-blist-update-node (proc name id status args)
  (let ((store      (elim-fetch-process-data proc :blist))
        (bnode-uid  (elim-avalue "bnode-uid" args))
        (bnode-cons  nil))
    (if (setq bnode-cons (assoc bnode-uid store))
        (setcdr bnode-cons args)
      (setq store (cons (cons bnode-uid args) store))
      (elim-store-process-data proc :blist store))))

(defun elim-blist-create-node (proc name id status args)
  (let ((store      (elim-fetch-process-data proc :blist))
        (bnode-uid  (elim-avalue "bnode-uid" args))
        (bnode-cons  nil))
    (if (setq bnode-cons (assoc bnode-uid store))
        nil ;; noop, we already have an entry so don't touch it.
      (setq store (cons (cons bnode-uid args) store))
      (elim-store-process-data proc :blist store))))

(defun elim-blist-remove-node (proc name id status args)
  (let ((store      (elim-fetch-process-data proc :blist))
        (bnode-uid  (elim-avalue "bnode-uid" args))
        (bnode-cons  nil))
    (when (setq bnode-cons (assoc bnode-uid store))
      (assq-delete-all (car bnode-cons) store)) ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; daemon to client request handlers (requests are calls that require a
;; structured response and UI interaction with the user)
(defvar elim-form-ui-data nil)
(defvar elim-form-ui-args nil)

(defun elim-form-ui-ok-cb (&optional parent child event &rest stuff) 
  (when elim-form-ui-args
    (let (value item-name item-value proc name id args response)
      (mapc
       (lambda (W) 
         (setq item-name  (widget-get W 'ident) 
               item-value (widget-value W))
         (when item-name
           (message "widget %s == %S" item-name item-value)
           (setq value (cons (elim-atom-to-item item-name item-value) value))))
       elim-form-ui-data)
      (setq proc (cadr (memq :process elim-form-ui-args))
            name (cadr (memq :name    elim-form-ui-args))
            id   (cadr (memq :call-id elim-form-ui-args)))
      (setq response 
            (elim-daemon-response name id 0 (nconc (list 'alist nil) value)))
      (elim-process-send proc response)) 
    (kill-buffer nil) ))

(defun elim-form-ui-nok-cb (&optional parent child event &rest stuff) 
  (when elim-form-ui-args
    (let (proc name id response)
      (setq proc (cadr (memq :process elim-form-ui-args))
            name (cadr (memq :name    elim-form-ui-args))
            id   (cadr (memq :call-id elim-form-ui-args)))
      (setq response (elim-daemon-response name id -1 (list 'alist nil)))
      (elim-process-send proc response))
    (kill-buffer nil) ))

(defun elim-form-ui-handle-event (&optional parent child event &rest stuff)
  (widget-put parent :value (widget-value parent)))

(defun elim-form-widget-create (type ident &rest args)
  "Create an emacs widget, and record it in `elim-form-ui-data'."
  (let ((widget nil))
    (setq widget            (apply 'widget-create type args)
          elim-form-ui-data (cons widget elim-form-ui-data))
    (widget-put widget 'ident ident)
    widget))

(defun elim-request-field-string (id data)
  (elim-form-widget-create 'string 
                           id
                           :notify 'elim-form-ui-handle-event
                           :tag    (or (elim-avalue "label" data) id)
                           :size   20
                           :secret (and (elim-avalue "masked" data) ?*)
                           (or (elim-avalue "value"   data)
                               (elim-avalue "default" data) "")))

(defun elim-request-field-choice (id data)
  (let ((idx 0) (item nil))
    (apply 'elim-form-widget-create 
           'choice
           id
           :notify 'elim-form-ui-handle-event
           :tag    (or (elim-avalue "label" data) id)
           :value  (or (elim-avalue "value"   data)
                       (elim-avalue "default" data) 0) 
           (mapcar
            (lambda (o)
              (setq item (list 'const :tag o idx) idx (1+ idx)) item)
            (elim-avalue "choices" data))) ))

(defun elim-request-field-toggle (id data)
  (let ((idx 0) (item nil))
    (elim-form-widget-create 'boolean
                             id
                             :notify 'elim-form-ui-handle-event
                             :tag    (or (elim-avalue "label" data) id)
                             (or (elim-avalue "value"   data)
                                 (elim-avalue "default" data) nil)) ))

(defun elim-request-field-number (id data)
  (elim-form-widget-create 'integer
                           id
                           :notify 'elim-form-ui-handle-event
                           :tag    (or (elim-avalue "label" data) id)
                           (or (elim-avalue "value"   data)
                               (elim-avalue "default" data) 0)))

(defun elim-request-field (field)
  (let ((id   (car field))
        (data (cdr field)) type)
    (setq type (elim-avalue "type" data))
    (cond ((eq type :string ) (elim-request-field-string id data))
          ((eq type :boolean) (elim-request-field-toggle id data))
          ((eq type :integer) (elim-request-field-number id data))
          ((eq type :choice ) (elim-request-field-choice id data))
          (t (widget-insert (format "Unsupported type: %S %s" id type)))) 
    (widget-insert "\n")))

(defun elim-request-field-group (group)
  (let ((label  (car group)) 
        (fields (cdr group))) 
    (widget-insert label ":\n")
    (mapc 'elim-request-field fields)
    (widget-insert "====================================================\n")))

(defun elim-request-fields (proc name id status args)
  (let (fields field-buf buf-name ok-label nok-label)
    (setq fields    (elim-avalue "fields" args)
          buf-name  (concat "* " (or (elim-avalue "title"        args)
                                     (elim-avalue "primary"      args)
                                     (elim-avalue "who"          args)
                                     (elim-avalue "account-name" args) 
                                     "Information Required:"         ) " *")
          field-buf (generate-new-buffer buf-name))
    (with-current-buffer field-buf 
      (kill-all-local-variables)
      (let ((inhibit-read-only t)) (erase-buffer))
      (remove-overlays)
      (make-local-variable 'elim-form-ui-data)
      (make-local-variable 'elim-form-ui-args)
      (setq elim-form-ui-args (list :process proc 
                                    :name    name 
                                    :call-id id 
                                    :args    args))
      (widget-insert (concat (or (elim-avalue "primary" args)
                                 (elim-avalue "title"   args)) "\n"
                             (elim-avalue "secondary" args)))
      ;; actually create the widget:
      (mapc 'elim-request-field-group fields)
      (setq ok-label  (elim-avalue "ok-label"  args)
            nok-label (elim-avalue "nok-label" args))
      (elim-form-widget-create 'push-button
                               nil
                               :format (format "[%%[%s%%]]" nok-label)
                               :notify 'elim-form-ui-nok-cb)
      (widget-insert " ")
      (elim-form-widget-create 'push-button
                               nil
                               :format (format "[%%[%s%%]]" ok-label)
                               :notify 'elim-form-ui-ok-cb)
      (use-local-map widget-keymap)
      (widget-setup)
      (beginning-of-buffer)
      (widget-forward 1))
    (display-buffer field-buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; daemon calls not intended for direct client use:
(defun elim-init (process &optional ui-string user-dir) 
  "Initialises the elim daemon. Not normally called by the user, 
as it relies on initialisation done by `elim-start'."
  (let ((init-call nil)
        (dummy     nil)
        (uiid     (or ui-string "elim"))
        (dir      (or user-dir  elim-directory))
        (arglist   nil))
    (setq arglist    (list         "dot-dir" dir "ui-id" uiid)
          arglist    (elim-simple-list-to-proto-alist arglist)
          proto-call (elim-daemon-call  'init   nil   arglist))
    (elim-process-send process proto-call) ))

(defun elim-load-enum-values (process)
  (elim-process-send process
    (elim-daemon-call 'enumerations nil '(alist nil)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-daemon functions intended for the user, and general utilities
(defun elim-protocol-alist (process)
  "Fetches the cached alist of im protocol ids and names supported by PROCESS."
  (elim-fetch-process-data process :protocols))

(defun elim-protocol-supported (process protocol)
  "Returns nil if PROTOCOL is not in the cache of supported im protocols, 
or a cons of \(\"protocol-id\" . \"protocol-name\") if it is."
  (or (assoc  protocol                 (elim-protocol-alist process))
      (assoc (concat "prpl-" protocol) (elim-protocol-alist process))))

(defun elim-avalue (key alist) (cdr (assoc key alist)))

(defun elim-assoc  (key list &optional test test-cons)
  (let ((slot nil) (pred (or test 'equal)))
    (mapc 
     (lambda (C)
       (when (ignore-errors (funcall pred key (if test-cons C (car C))))
         (setq slot C))) list) slot))

(defun elim-account-data (process account)
  "Given an ACCOUNT (an account name string or account uid number), return
its data in the form (uid (:key . value) ...). :key items should include
:name and :proto, but others may also be present."
  (let ((accounts (elim-fetch-process-data process :accounts))) 
    (cond ((numberp account) (elim-assoc account accounts '=))
          ((stringp account)
           (elim-assoc account accounts 
                       (lambda (K C)
                         (equal K (cdr (assq :name (cdr C))))) t))
           (t (elim-debug "account %S not found" account)     nil)) ))

(defun elim-account-proto-items (process account)
  (let ((adata (elim-account-data process account)) (arglist nil))
    (when adata 
      (list 
       (elim-atom-to-item "account-uid"  (car adata))
       (elim-atom-to-item "account-name" (cdr (assq :name  (cdr adata))))
       (elim-atom-to-item "account-name" (cdr (assq :proto (cdr adata))))) )))

(defun elim-buddy-list (process)
  (elim-fetch-process-data process :blist))

(defun elim-buddy-children (process uid)
  (let (cdata children (seen (list uid)) cuid)
    (mapc 
     (lambda (X)
       (setq cuid (car X) cdata (cdr X))
       (when (not (member cuid seen))
         (setq seen (cons cuid seen))
         (when (eql (elim-avalue "bnode-parent" cdata) uid)
           (setq children (cons X children)))))
     (elim-fetch-process-data process :blist))
    children))

(defun elim-buddy (process uid)
  (elim-avalue uid (elim-fetch-process-data proc :blist)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elim daemon calls:

(defun elim-start (&optional ui-string user-dir client-ops)
  "Starts up an elim daemon with the standard location and command, and 
initialises it (setq up the elim user directory, loads the list of 
supported protocols asynchronously). Returns the elim object (a process)
which is the first parameter for most calls intended for client libraries
to use. \n
UI-STRING is a parameter (a string) which libpurple uses to identify
the libpurple instance internally. It will be initialised to something 
suitable if you do not supply it (or set it to nil).\n
USER-DIR is the directory in which more-or-less permanent user data
\(buddy lists, account details and so forth) will be stored. It will also
be initialised to the value of `elim-directory' if you do not supply it."
  (let ( (buf (generate-new-buffer "*elim*"))
         (process-connection-type nil)
         (elim-initialising         t)
         (elim                    nil) )
    (setq elim (start-process-shell-command
                (buffer-name buf) nil (elim-command)))
    (elim-store-process-data elim :client-ops      client-ops)
    (elim-store-process-data elim :protocol-buffer buf)
    (elim-store-process-data elim :initialised     0)
    (set-process-filter elim 'elim-input-filter) 
    (elim-init elim ui-string user-dir)
    (elim-update-protocol-list elim)
    (elim-update-account-list  elim)
    (elim-load-enum-values     elim)
    (while (< (elim-fetch-process-data elim :initialised) 3)
      (accept-process-output))
    elim))

(defun elim-update-account-list (process)
  "Update (asynchronously) the IM account list cache."
  (elim-process-send process (elim-daemon-call 'list-accounts nil nil)))

(defun elim-update-protocol-list (process)
  "Updates (asynchronously) the alist of im protocol id's and names supported
by PROCESS. Not normally useful to the user."
  (elim-process-send process (elim-daemon-call 'list-protocols nil nil)))

(defun elim-add-account (process account protocol password &optional options)
  "Given an elim PROCESS, add the account defined by 
ACCOUNT (string, eg \"foo@irc.freenode.net\")
PROTOCOL (eg \"prpl-irc\")
PASSWORD (a string or nil)
and an optional list of strings:
OPTIONS (\"key\" \"value\" ...)."
  (if (elim-protocol-supported process protocol)
      (let (arglist optitem)
        (setq optitem (when options (elim-simple-list-to-proto-alist options))
              optitem (when optitem (elim-sexp-to-item "options" optitem))
              arglist (list "account-name" account
                            "im-protocol"  protocol
                            "password"     password)
              arglist (elim-simple-list-to-proto-alist arglist))
        (when options (setq arglist (nconc arglist (list optitem))))
        (elim-process-send process (elim-daemon-call 'add-account nil arglist))
        t)
    (error "Unsupported IM protocol: %s" protocol)))

(defun elim-add-buddy (process account buddy &optional group)
  "Given an elim PROCESS an ACCOUNT name or uid and a BUDDY (im screen name),
add that user to your buddy list"
  (let ((arglist (elim-account-proto-items process account)))
    (if arglist
        (progn
          (setq arglist (nconc (list 'alist nil)
                               (elim-atom-to-item "buddy-name" buddy)
                               (elim-atom-to-item "group" (or group "Buddies"))
                               arglist))
          (elim-process-send process (elim-daemon-call 'add-buddy nil arglist)))
      (error "No such account: %s" account)) ))

(defun elim-co-disco (process account operation)
  (let ((arglist (elim-account-proto-items process account)))
    (when arglist
      (setq arglist (nconc (list 'alist nil) arglist))
      (elim-process-send process (elim-daemon-call operation nil arglist)) )))

(defun elim-disconnect (process account)
  "Disconnect from ACCOUNT (a uid (number) or string (account name))."
  (elim-co-disco process account 'disconnect))

(defun elim-connect (process account)
  "Connect to ACCOUNT (a uid (number) or string (account name))."
  (elim-co-disco process account 'connect))

(defun elim-message (process account conversation text)
  "Send a TEXT to ACCOUNT (name or uid) CONVERSATION (uid) via elim PROCESS"
  (let (acct-args arglist conv-arg)
    (setq acct-args (elim-account-proto-items  process account)
          conv-arg  (elim-atom-to-item "conv-uid" conversation)
          text-arg  (elim-atom-to-item "text"     text        )
          arglist   (nconc (list 'alist nil conv-arg text-arg) acct-args))
    (if acct-args
        (elim-process-send process
                           (elim-daemon-call 'message nil arglist))
        (error "No such account: %S" account)) ))

(defun elim-join-chat (process account alias options)
  ""
  (let (alias-arg optitems acct-args arglist)
    (setq optitems  (elim-simple-list-to-proto-alist  options )
          optitems  (elim-sexp-to-item "chat-options" optitems)
          acct-args (elim-account-proto-items process account )
          alias-arg (elim-atom-to-item "chat-alias"   alias   )
          arglist   (nconc (list 'alist nil alias-arg optitems) acct-args))
    (elim-process-send process (elim-daemon-call 'join-chat nil arglist)) ))

(defun elim-chat-parameters (process protocol)
  "Given an elim PROCESS and a PROTOCOL id, return an alist giving details
on the parameters required to join a chat/room/channel on that PROTOCOL."
  (when (elim-protocol-supported process protocol)
    (let ((id       (elim-call-id))
          (response nil)
          (args     nil)
          (arglist  (list 'alist nil 
                          (elim-atom-to-item "im-protocol" protocol))))
      (elim-set-resp-handler-by-id process id 'elim-store-response-by-id)
      (elim-process-send process
                         (elim-daemon-call 'chat-params id arglist))
      (while (not (setq response (elim-fetch-response-by-id process id)))
        (accept-process-output))
      ;;(message "response for id %s received: %S" id response)
      (setq args (nth 3 response))
      (if (equal (elim-avalue "status" args) 0)
        (elim-avalue "parameters" (elim-avalue "value" args)) nil) )))

