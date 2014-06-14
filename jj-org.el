(provide 'jj-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode

;; (require 'org-trello)
;; to have org-trello activated for each org file, uncomment this
;; (add-hook 'org-mode-hook 'org-trello-mode)
;; otherwise, M-x org-trello-mode

(custom-set-variables
 '(org-babel-load-languages (quote ((emacs-lisp . t) (R . t))))
 '(org-confirm-babel-evaluate nil))

(define-skeleton org-skeleton
  "Header info for a emacs-org file."
  "Title: "
  "#+TITLE:" str " \n"
  "#+AUTHOR: Jonas Jarutis\n"
  "#+email: jonas@vinted.com\n"
  "#+INFOJS_OPT: \n"
  "#+PROPERTY: header-args:R  :session *R*\n"
  "#+PROPERTY: header-args:R  :cache yes\n"
  "#+PROPERTY: header-args:R  :results output graphics\n"
  "#+PROPERTY: header-args:R  :exports both\n"
  "#+PROPERTY: header-args:R  :tangle yes\n"
  "-----"
 )
(global-set-key [C-S-f4] 'org-skeleton)
(global-set-key [C-c C-x p] 'org-set-property)

(setq org-tags-column 80)

(eval-after-load "org"
  '(require 'ox-md nil t))

