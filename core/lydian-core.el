;;; lydian-core.el -- core configuration.

;;; Commentary:

;;; Code:

;; These packages are used everywhere
(use-package s)
(use-package f)
(use-package diminish)
(use-package bind-key)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defun wrap-with (s)
    "Create a wrapper function for smartparens using S."
    `(lambda
         (&optional
             arg)
         (interactive "P")
         (sp-wrap-with-pair ,s)))

(provide 'lydian-core)

;;; lydian-core.el ends here
