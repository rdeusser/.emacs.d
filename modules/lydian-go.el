;;; lydian-go.el -- Go config.

;;; Commentary:

;;; Code:

(use-package go-mode
  :config
  (setenv "GOPATH" (concat (getenv "HOME") "/go"))
  (add-to-list 'auto-mode-alist '("\\.golden\\'" . text-mode))
  (define-key 'help-command (kbd "G") 'godoc)
  (define-key go-mode-map (kbd "C-c b") 'go-run)
  (define-key go-mode-map (kbd "C-h f") 'godoc-at-point)
  ;; Prefer goimports to gofmt if installed.
  (let ((goimports (executable-find "goimports")))
    (when goimports
      (setq gofmt-command goimports)))
  ;; Prefer gofumpt to goimports if installed.
  (let ((gofumpt (executable-find "gofumpt")))
    (when gofumpt
      (setq gofmt-command gofumpt)))
  (use-package company-go)
  (use-package go-eldoc
    :config
    (go-eldoc-setup))
  (use-package go-projectile)
  ;; Set up before-save hooks to format buffer and add/delete imports.
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

;; Add tags to structs.
(use-package go-add-tags
  :config
  (custom-set-variables
   '(go-add-tags-style 'lower-camel-case))
  (define-key go-mode-map (kbd "C-c t") #'go-add-tags))

(provide 'lydian-go)

;;; lydian-go.el ends here
