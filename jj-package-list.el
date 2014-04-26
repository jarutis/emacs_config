(provide 'jj-package-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list of packages in use

(ensure-package-installed 'solarized-theme 'projectile 'smex 'magit)

;; activate installed packages
(package-initialize)
