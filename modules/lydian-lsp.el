;;; lydian-lsp.el --- lsp-mode setup

;;; Commentary:

;;; Code:

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :config
  (setq lsp-go-use-gofumpt t)
  (setq lsp-enable-semantic-highlighting t)
  (defalias 'fill 'lsp-execute-code-action))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-psoition 'at-point))

(provide 'lydian-lsp)

;;; lydian-lsp.el ends here
