;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Startup and Behavior Controls 
;; http://tychoish.com/documentation/managing-emacs-configuraiton-and-lisp-systems/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path (cons "~/emacs/config" load-path)) 
(setq load-path (cons "~/emacs" load-path)) 

(setq custom-file "~/emacs/custom.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Machine Specific Configuration Section

(if (file-directory-p "~/emacs/backup")
    (setq backup-directory-alist '(("." . "~/emacs/backup")))
  (message "Directory does not exist: ~/emacs/backup"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the real init

(require 'jj-init)
;; database config
(require 'jj-database)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ess specific paths

(setq inferior-julia-program-name "/Users/jarutis/dev/julia/usr/bin/julia-basic")
(setq inferior-R-program-name "/usr/local/bin/R")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; latex
(setenv "PATH" (concat "/usr/texbin:/usr/local/bin/mysql:/usr/local/bin:"
(getenv "PATH")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; copy paste osx

(when (eq system-type 'darwin)
  (unless (display-graphic-p)
    ;; Make sure cut/paste works properly. Gotten from:
    ;; http://mindlev.wordpress.com/2011/06/13/emacs-in-a-terminal-on-osx/#comment-20
    (defun copy-from-osx ()
      "Copies the current clipboard content using the `pbcopy` command"
      (shell-command-to-string "pbpaste"))

    (defun paste-to-osx (text &optional push)
      "Copies the top of the kill ring stack to the OSX clipboard"
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))

    ;; Override defaults to use the mac copy and paste
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx)))
