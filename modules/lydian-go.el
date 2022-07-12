;;; lydian-go.el -- Go config.

;;; Commentary:

;;; Code:

(use-package go-mode
    :config
    ;; some initial keybindings
    (define-key 'help-command (kbd "G") 'godoc)
    (define-key go-mode-map (kbd "C-c b") 'go-run)
    (define-key go-mode-map (kbd "C-h f") 'godoc-at-point)
    ;; Prefer goimports to gofmt if installed
    (let ((goimports (executable-find "goimports")))
        (when goimports
            (setq gofmt-command goimports)))
	(use-package company-go)
    (use-package go-eldoc
        :config
        (go-eldoc-setup))
    (use-package go-projectile))

(provide 'lydian-go)

;;; lydian-go.el ends here
