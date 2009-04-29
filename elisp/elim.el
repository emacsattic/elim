
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

(defvar elim-call-handler-alist
  '(;; account ops
    (elim-account-notify-added  )
    (elim-account-status-changed)
    (elim-account-request-add   )
    (elim-account-request-auth  )
    ;; blist (buddy list) ops
    (elim-blist-insert-node     )
    (elim-blist-remove-node     )
    (elim-blist-update-node     )
    ;; connection ops
    (elim-connection-status     )
    (elim-connection-progress   )
    (elim-disconnect-reason     )
    ;; network status
    (elim-network-up            )
    (elim-network-down          )
    ;; conversation
    (elim-conv-create           )
    (elim-conv-destroy          )
    (elim-conv-write-chat       )
    (elim-conv-write-im         )
    (elim-conv-write-sys        ) )
  "Alist of function call handlers. The car of a given element is the 
elim protocol function symbol. The cdr is the handler function, or nil
if the symbol to look for is the same as that of the protocol function.")

(defvar elim-resp-handler-alist
  '((add-account    . elim-response-filter-account-data)
    (add-buddy      . nil)
    (connect        . nil)
    (debug          . nil)
    (disconnect     . nil)
    (init           . elim-init-response)
    (list-protocols . elim-list-protocols-response)
    (message        . nil))
  "Alist of function response handlers. The car of a given element is the
elim protocol function symbol. The cdr is the handler function, or nil
if we do not intend to wait for the response (this is the usual case).")

(defvar elim-call-id          0)
(defvar elim-proto-buffer   nil)
(defvar elim-process        nil)
(defvar elim-callback-stack nil)
(defvar elim-conversations  nil)
(defvar elim-accounts       nil)

(defun elim-call-id ()
  (number-to-string (setq elim-call-id (1+ elim-call-id))))

(defun elim-command () 
  (format "%s 2>> %s/elim.$$.stderr" elim-executable elim-directory))
; (format "strace -f -o /tmp/elim.strace %s" elim-executable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protocol parsing and formatting:
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
          (cond ((eq type 'string) (elim-html-to-text       (car value)))
                ((eq type 'int   ) (string-to-number        (car value)))
                ((eq type 'float ) (string-to-number        (car value)))
                ((eq type 'bool  ) (/= 0 (string-to-number (car value))))
                ((eq type 'data  ) (base64-decode-string    (car value)))
                ((eq type 'list  ) (mapcar 'elim-parse-proto-args value))
                ((eq type 'alist ) (mapcar 'elim-parse-proto-args value))
                ;; this type was an old form of the protocol, no longer used:
                ((eq type 'item  ) (cons 
                                    (cdr (assq 'name attr)) 
                                    (elim-parse-proto-args (car value))))
                (t                 (error "Bad elim arg type : %S" type)) )) 
    (if name (cons name parsed) parsed)))
  
(defun elim-entity-to-string (match)
  (format "%c" (string-to-int (match-string 1 match))))

;; this is just a temporary hack, replace it with something better
(defun elim-html-to-text (string)
  "Return a copy of STRING with the major named entities and numeric (byte)
entities converted into characters."
  (replace-regexp-in-string "&#\\([0-9]+\\);" 'elim-entity-to-string
   (replace-regexp-in-string "&apos;" "'"
    (replace-regexp-in-string "&quot;" "\""
     (replace-regexp-in-string "&amp;" "&"
      (replace-regexp-in-string "&lt;" "<"
       (replace-regexp-in-string "&gt;" ">" string)) )) )))

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
    (cond ((stringp  x) (list            'string attr x))
          ((numberp  x) (elim-number-to-proto    x attr))
          ((booleanp x) (list 'bool attr (if x "1" "0"))) )))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun elim-debug (&rest args)
  (message (apply 'format args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun elim-handle-sexp (proc sexp)
  (ignore-errors
    (let ((type (car  sexp))
          (name (caar (cddr sexp)))
          (attr (car  (cdar (cddr sexp))))
          (args (cadr (cddr sexp))) )
      (setq args (elim-parse-proto-args args))
      (message "%S: (%S %S %S)" type name attr args)
      (cond 
       ((eq type 'function-call    ) (elim-handle-call proc name attr args))
       ((eq type 'function-response) (elim-handle-resp proc name attr args))
       (t (message "unknown protocol envelope: %S" type))) )))

;; if we implement setting up response handlers for individual call instances
;; via elim-callback-stack, this is where we will fetch said handlers back:
(defun elim-get-handler-by-id   (proc id  ) nil)
(defun elim-get-handler-by-name (proc name)
  ;; in case the handler list has been localised for this process:
  ;;(set-buffer (process-buffer proc))
  (cdr (assq name elim-resp-handler-alist)))

(defun elim-handle-resp (proc name attr args)
  (let ( (id      (cdr (assq 'id attr))) 
         (handler  nil) )
    (setq handler (or (elim-get-handler-by-id   proc id  ) 
                      (elim-get-handler-by-name proc name)))
    ;; if told to use a handler but there's not a handler with that name:
    (when (and handler (not (fboundp handler)))
      (setq handler 'elim-default-response-handler))
    ;; finally, we must have a valid handler symbol to proceed:
    (when handler (funcall handler proc name id attr args)) ))

(defun elim-handle-call (proc name attr args)
  (message "elim call %S" name)
  )

(defun elim-input-filter (process data)
  (let ((pt nil) (sexp nil) (read-error nil) (sexp-list nil))
    (set-buffer (elim-fetch-process-data process 'protocol-buffer))
    (setq pt    (point))
    (goto-char  (point-max))
    (insert      data)
    (goto-char   pt)
    (condition-case read-error
        (while (setq sexp (read (current-buffer)))
          (setq sexp-list (cons sexp sexp-list))
          (delete-region pt (point)))
      (error
       (message "read-error: %S" read-error)
       (goto-char pt)))
    ;;(message "sexp-list:\n\n%S\n" sexp-list)
    (when sexp-list 
      ;;(setq sexp-list (nreverse sexp-list))
      (mapcar (lambda (S) (elim-handle-sexp process S)) sexp-list)) ))

(defun elim-process-send (process sexp-value)
  (let ((print-level  nil) 
        (print-length nil)
        (sexp-string  nil))
    (setq sexp-string (prin1-to-string sexp-value))
    (process-send-string process sexp-string)
    (message "sent %s" sexp-string)
    (accept-process-output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fetching and storing per-daemon data:
(defun elim-store-process-data (proc key value)
  (let ((elim-data (process-get proc 'elim-data)))
    (setcdr (or (assq key elim-data)
                (car (setq elim-data (cons (cons key nil) elim-data)))) value)
    (process-put proc 'elim-data elim-data)))

(defun elim-fetch-process-data (proc key)
  (cdr (assq key (process-get proc 'elim-data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; daemon response handlers:
(defun elim-default-fail-handler (proc name id attr args)
  (warn "%s<%s> failed (%S)" name id args))

(defun elim-default-response-handler (proc name id attr args)
  (if (equal (cdr (assoc "status" args)) 0)
      (elim-debug "%s<%s> successful %S" name id (cdr (assoc "value" args)))
    (elim-default-fail-handler proc name id attr args)))

(defun elim-list-protocols-response (proc name id attr args)
  (if (not (equal (cdr (assoc "status" args)) 0)) 
      (elim-default-fail-handler proc name id attr args)
    (elim-store-process-data proc 'protocols (cdr (assoc "value" args))) ))

(defun elim-response-filter-account-data (proc name id attr args)
  (let (uid proto name store adata (val (cdr (assoc "value" args))))
    (when (setq uid (cdr (assoc "account-uid" val)))
      (setq store (elim-fetch-process-data proc 'accounts))
      (when (not (assoc uid store))
        (setq name  (cdr (assoc "account-name" val))
              proto (cdr (assoc "im-protocol"  val))
              adata (cons uid (list (cons :name  name ) 
                                    (cons :proto proto)))
              store (cons adata store))
        (message "storing: %S" store)
        (elim-store-process-data proc 'accounts store)))
      (or adata uid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; daemon calls not intended for direct client use:
(defun elim-init (process &optional ui-string user-dir) 
  "Initialises the elim daemon. Not normally called by the user, 
as it relies on initialisation done by `elim-start'."
  (message "ELIM INIT ENTERED")
  (let ((init-call nil)
        (dummy     nil)
        (uiid     (or ui-string "elim"))
        (dir      (or user-dir  elim-directory))
        (arglist   nil))
    (message "ELIM INIT ENTERED 2")
    (setq arglist    (list         "dot-dir" dir "ui-id" uiid)
          ;;dummy      (message "ELIM INIT ARGLIST %S arglist" arglist)
          arglist    (elim-simple-list-to-proto-alist arglist)
          ;;dummy      (message "ELIM INIT ARGLIST 2")
          proto-call (elim-daemon-call  'init   nil   arglist)
          ;;dummy      (message "ELIM INIT DCALL   1")
          )
    (message "elim-init: proto-call: %S" proto-call)
    (elim-process-send process proto-call) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-daemon functions intended for the user, and general utilities
(defun elim-protocol-alist (process)
  "Fetches the cached alist of im protocol ids and names supported by PROCESS."
  (elim-fetch-process-data process 'protocols))

(defun elim-protocol-supported (process protocol)
  "Returns nil if PROTOCOL is not in the cache of supported im protocols, 
or a cons of \(\"protocol-id\" . \"protocol-name\") if it is."
  (assoc protocol (elim-protocol-alist process)))

(defun elim-assoc (key list &optional test test-cons)
  (let ((slot nil) (pred (or test 'equal)))
    (mapc 
     (lambda (C)
       (when (ignore-errors (funcall pred key (if test-cons C (car C))))
         (setq slot C))) list) slot))

(defun elim-account-data (process account)
  "Given an ACCOUNT (an account name string or account uid number), return
its data in the form (uid (:key . value) ...). :key items should include
:name and :proto, but others may also be present."
  (let ((accounts (elim-fetch-process-data process 'accounts))) 
  ;;   (let ((accounts '((1073741824.0 (:name . "foo@bar.org")
  ;;                                   (:proto . "prpl-moose"))
  ;;                     (1334324      (:name . "foo@blerg.x") 
  ;;                                   (:proto . "prpl-moose"))) ))
    (cond ((numberp account) (elim-assoc account accounts '=))
          ((stringp account)
           (elim-assoc account accounts 
                       (lambda (K C)
                         (equal K (cdr (assq :name (cdr C))))) t))
           (t (message "account %S not found" account)) )))

(defun elim-account-proto-items (process account)
  (let ((adata (elim-account-data process account)) (arglist nil))
  ;;(let ((adata '(1073741824.0 (:name . "foo@bar.org")
  ;;                            (:proto . "prpl-moose")) ))
    (when adata 
      (list 
       (elim-atom-to-item "account-uid"  (car adata))
       (elim-atom-to-item "account-name" (cdr (assq :name  (cdr adata))))
       (elim-atom-to-item "account-name" (cdr (assq :proto (cdr adata))))) )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elim daemon calls:

(defun elim-start (&optional ui-string user-dir)
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
  (let ( (buf       (generate-new-buffer "*elim*"))
         (proto-buf (generate-new-buffer "*elim-proto*"))
         (process-connection-type nil)
         (elim                    nil) )
    (setq elim (start-process-shell-command 
                (buffer-name buf) buf (elim-command)))
    ;;(message "STARTED ELIM")
    (elim-store-process-data elim 'protocol-buffer proto-buf)
    (set-process-filter elim 'elim-input-filter) 
    ;;(message "SET FILTER")
    (elim-init elim ui-string user-dir)
    ;;(message "CALLED INIT")
    (elim-update-protocol-list elim) 
    ;;(message "CALLED PROTO UPDATE")
    (sit-for 1)
    (message "DONE")
    elim))

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
    (ignore "debug here")))

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

(defun elim-join-chat (process account alias options)
  ""
  (let (alias-arg optitems acct-args arglist)
    (setq optitems  (elim-simple-list-to-proto-alist  options )
          optitems  (elim-sexp-to-item "chat-options" optitems)
          acct-args (elim-account-proto-items process account )
          alias-arg (elim-atom-to-item "chat-alias"   alias   )
          arglist   (nconc (list 'alist nil alias-arg optitems) acct-args))
    (elim-process-send process (elim-daemon-call 'join nil arglist)) ))
