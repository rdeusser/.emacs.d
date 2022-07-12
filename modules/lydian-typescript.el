;;; lydian-typescript --- typescript-mode configuration.

;;; Commentary:

;;; Code:

;; Typescript
(use-package typescript-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

  (defun lsp-typescript-before-apply-edits-hook ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t))

  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'lsp-before-apply-edits-hook #'lsp-typescript-before-apply-edits-hook))

(provide 'lydian-typescript)

;;; lydian-typescript.el ends here
