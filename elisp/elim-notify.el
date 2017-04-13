;; Copyright © 2010 Vivek Dasmohapatra 

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
(require 'dbus-util)

(defconst elim-notify-service   "org.freedesktop.Notifications"
  "The dbus service providing user notifications.")
(defconst elim-notify-interface "org.freedesktop.Notifications"
  "The dbus interface providing user notifications.")
(defconst elim-notify-path
  (replace-regexp-in-string "\\.\\|^" "/" elim-notify-service)
  "The path at which we expect to find `elim-notify-interface'")

(defvar elim-notify-interface-spec nil)
(defvar elim-elim-notify-action-invoked nil)
(defvar elim-notify-notice-closed  nil)
(defvar elim-notify-notifications  nil)

(defun elim-notify-action (id action)
  (let ((handler (assq id elim-notify-notifications)) callback)
    (when handler
      (if (setq callback (cdr handler)) (funcall callback action))
      ;;(message "deleting handler for %S after invocation" id)
      (setq elim-notify-notifications (assq-delete-all id elim-notify-notifications)) )))

(defun elim-notify-closed (id &rest ignored)
  (let ((handler (assq id elim-notify-notifications)) callback)
    (when handler
      ;;(message "deleting handler for %S after close" id)
      (setq elim-notify-notifications (assq-delete-all id elim-notify-notifications)) )))

(defun elim-notify-init ()
  "Idempotently initialise the `elim-notify-service' interface and handlers.
Returns t if the interface was found, nil otherwise."
  (or elim-notify-interface-spec
      (setq elim-notify-interface-spec
            (dbus-util-introspect-service elim-notify-service)))
  (or elim-elim-elim-notify-action-invoked
      (setq elim-elim-elim-notify-action-invoked
            (dbus-register-signal :session
                                  elim-notify-service
                                  elim-notify-path
                                  elim-notify-interface
                                  "ActionInvoked"
                                  'elim-notify-action)))
  (or elim-notify-notice-closed
      (setq elim-notify-notice-closed
            (dbus-register-signal :session
                                  elim-notify-service
                                  elim-notify-path
                                  elim-notify-interface
                                  "NotificationClosed"
                                  'elim-notify-closed)))
  (if elim-notify-interface-spec t nil))

(defun elim-notify-exit ()
  "Idempotently uninitialise the `elim-notify-service' interface and unregister any
related handlers."
  (mapc (lambda (obj) (and obj (dbus-unregister-object obj))
          (list elim-elim-elim-notify-action-invoked elim-notify-notice-closed)))
  (setq elim-notify-interface-spec nil
        elim-elim-elim-notify-action-invoked nil
        elim-notify-notice-closed  nil))

(defun elim-notify-show-message (&rest args)
  "Display a notification via the `elim-notify-service' dbus interface,
typically provided by notification-daemon.
The arguments are passed as a list of keys and values, with :symbols as
keys. The keys are mapped to the Notify method's arguments by stripping
the leading : and replacing \"-\" with \"_\".

::action-handler specifies the function to be called if you pass an action
or actions to the Notify method and one is invoked. This handler will be
called with one argument: The key from the action list which corresponds
to the button pressed.

Example:

  (elim-notify-show-message :id       0
                       ::action-handler (lambda (a) (message \"action: %S\" a))
                       :body     \"Time to Die.\"
                       :icon     \"/home/vivek/src/elim/icons/garak.png\"
                       :actions  '(\"some-action\" \"Action Button Label\")
                       :hints    nil
                       :timeout  30000
                       :app-name \"(elim . garak)\" 
                       :summary  \"wake up!\")

Since the arguments are named, you need not specify them in any order.
You do not need to specify optional arguments either."
  (when (elim-notify-init)
    (let ((action-handler (cadr (memq ::action-handler args))) return-handler)
      (setq return-handler
            `(lambda (id)
               (setq elim-notify-notifications
                     (cons (cons id ',action-handler) elim-notify-notifications))))
      (dbus-util-call-method :session
                             elim-notify-service nil elim-notify-interface "notify"
                             return-handler args))))

;; (elim-notify-show-message :id       0
;;                           ::action-handler 
;;                           (lambda (&rest args) (message "AH%S" args))
;;                           :body     "Time to Die."
;;                           :icon     "/home/vivek/src/elim/icons/garak.png"
;;                           :actions  '("foo" "bar")
;;                           :hints    nil
;;                           :timeout  30000
;;                           :app-name "elim" 
;;                           :summary  "wake up!")

(provide 'elim-notify)
