;;; lydian-company.el --- company-mode setup

;;; Commentary:

;;; Code:

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-lsp-cache-candidates 'auto)
  (setq company-lsp-async t)
  (setq company-lsp-enable-snippet t)
  (setq company-lsp-enable-recompletion t)
  (global-company-mode t))

(use-package company-dict)

(provide 'lydian-company)

;;; lydian-company.el ends here
