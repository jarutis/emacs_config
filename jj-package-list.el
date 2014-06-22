(provide 'jj-package-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list of packages in use

(ensure-package-installed 'solarized-theme 'projectile 'smex 'magit
                          'org-trello 'flx-ido 'auto-complete
			  'ess 'god-mode 'evil 'evil-leader 
                          'exec-path-from-shell)

;; activate installed packages
(package-initialize)
