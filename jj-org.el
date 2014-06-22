(provide 'jj-org)
(require 'org-install)
(require 'ob-tangle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode

;; (require 'org-trello)
;; to have org-trello activated for each org file, uncomment this
;; (add-hook 'org-mode-hook 'org-trello-mode)
;; otherwise, M-x org-trello-mode

(custom-set-variables
 '(org-babel-load-languages (quote ((emacs-lisp . t) (R . t) (sql . t))))
 '(org-confirm-babel-evaluate nil))

(define-skeleton org-skeleton
  "Header info for a emacs-org file."
  "Title: "
  "#+TITLE:" str " \n"
  "-----"
 )
(global-set-key [C-S-f4] 'org-skeleton)
(global-set-key [C-c C-x p] 'org-set-property)

(setq org-tags-column 80)

(eval-after-load "org"
  '(require 'ox-md nil t))

(setq org-babel-default-header-args
      (cons '(:noweb . "yes")
            (assq-delete-all :noweb org-babel-default-header-args)))

(setq org-babel-default-header-args:R
      (cons '(:session . "*R*")
            (assq-delete-all :session org-babel-default-header-args)))

(setq org-babel-default-header-args:emacs-lisp 
      (cons '(:results . "value")
            (assq-delete-all :results org-babel-default-header-args)))
