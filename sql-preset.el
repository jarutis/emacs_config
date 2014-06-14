;;; sql-preset.el --- SQL connection presets
;; -*- lexical-binding: t -*-

;; Copyright (C) 2013 Ian Eure

;; Author: Ian Eure <ian.eure@gmail.com>
;; Keywords: unix, processes, extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;; Example connection:
;;
;; (setq sql-preset-alist
;;       '((example
;;          (default-directory "~/")
;;          (sql-product 'mysql)
;;          (sql-user "username")
;;          (sql-password "password")
;;          (sql-database "database")
;;          (sql-server "host.tld")
;;          (sql-port 3306))))

;;; Code:

(require 'sql)

(defconst sql-preset-comint-exec (symbol-function 'comint-exec)
  "This saves the original `comint-exec', which is shadowed by
  sql-preset.")

(defvar sql-preset-alist
  '()

  "AList of preset connections for `sql-preset-connect'.")

(defvar sql-preset nil
  "The preset used to start this connection.")
(make-variable-buffer-local 'sql-preset)

(defun sql-preset-get-login (&rest what)
  "Return preset values for the current connection.

   This replaces SQL-GET-LOGIN in SQL-PRESET-CONNECT."
  (mapcar
   (lambda (sym)
     (let ((sym (if (listp sym) (car sym) sym)))
       (symbol-value (intern (format "sql-%s" (symbol-name sym))))))
   what))

(defun sql-presets-get ()
  "Return an alist of (SYMBOL . NAME) pairs for all defined SQL presets."
  (mapcar (lambda (conn) (cons (car conn) (symbol-name (car conn))))
          sql-preset-alist))

(defmacro sql-preset-with-conn (conn-vars &rest body)
  `(let ,conn-vars
     ,@body))

(defun sql-preset-hook-comint-exec (preset buffer &rest args)
  "Wrap `comint-exec', setting up buffer vars for sql-preset."
  (with-current-buffer buffer
    (set (make-variable-buffer-local 'comint-output-filter-functions)
         (cons 'sql-preset-send-password
               (remove 'comint-watch-for-password-prompt
                       comint-output-filter-functions)))
    (setq sql-preset preset))
  (apply sql-preset-comint-exec buffer args))

(defun sql-preset-connect (name &optional new)
  "Connect to a predefined SQL connection listed in `sql-connection-alist'"
  (interactive (list (intern (completing-read "Connection: "
                                              (sql-presets-get) nil t))
                     current-prefix-arg))
  (let* ((conn (cdr (assoc name sql-preset-alist)))
         (sql-name (symbol-name name)))
    (eval `(let ,conn
             ;; This throws a deprecation warning, but cl-flet is a
             ;; piece of shit that doesn't work, so fuck it.
             (flet ((sql-get-login (&rest what)
                                   (apply 'sql-preset-get-login what))
                    ;; Set up the buffer so we don't have to deal with passwords
                    (comint-exec (&rest args)
                                 (apply 'sql-preset-hook-comint-exec
                                        name
                                        args))
                    ;; Never reuse buffers, for now.
                    (sql-find-sqli-buffer (&optional product sql-connection)
                                          nil))
               ;; Set up the new buffer
               (with-current-buffer (get-buffer-create "*SQL*"))
               (with-current-buffer
                   (sql-product-interactive sql-product new)
                 (setq sql-preset name)))) t)))

(defun sql-preset-send-password (string)
  "Watch for a password prompt and send the contents of SQL-PASSWORD."
  (let ((sql-password (cadr
                       (assoc 'sql-password
                              (cdr (assoc sql-preset
                                          sql-preset-alist))))))
  (if (not (and sql-password (not (string= "" sql-password))
                (string-match comint-password-prompt-regexp string)))
      (comint-watch-for-password-prompt string)

    (let ((proc (get-buffer-process (current-buffer))))
      (if (not proc)
          (error "Buffer %s has no process" (current-buffer))
        (comint-snapshot-last-prompt)
        (funcall comint-input-sender proc sql-password))))))

(provide 'sql-preset)
;;; sql-preset.el ends here
