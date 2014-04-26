;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Startup and Behavior Controls 
;; http://tychoish.com/documentation/managing-emacs-configuraiton-and-lisp-systems/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path (cons "~/emacs" load-path)) 

(setq custom-file "~/emacs/custom.el")
;; (add-to-list 'load-path "~/emacs/snippet/")
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/") 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Machine Specific Configuration Section
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (file-directory-p "~/emacs/backup")
    (setq backup-directory-alist '(("." . "~/emacs/backup")))
  (message "Directory does not exist: ~/emacs/backup"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Load the real init
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'jj-init)
