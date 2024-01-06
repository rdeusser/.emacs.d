;;; go --- go configuration.

;;; Commentary:

;;; Functions:

(defun gofmt ()
  "Formats the current buffer or text selection. Respects `narrow-to-region'"
  (interactive)
  (message "Running gofumpt")
  (setq current-point (point))
  (if (use-region-p)
      (call-process-region start end "gofumpt" t t t)
    (call-process-region (point-min) (point-max) "gofumpt" t t t))
  (goto-char current-point))

(defun go/before-save-hook ()
  "Runs gofumpt on the buffer before saving."
  (add-hook 'before-save-hook #'gofmt -10 t))

;;; Hooks:

;;; Code:

(use-package go-ts-mode
  :straight nil
  :hook (go-ts-mode . go/before-save-hook)
  :bind (:map go-ts-mode-map
              ("C-c l f" . #'gofmt))
  :init
  (setq go-ts-mode-indent-offset 4))

(use-package go-tag
  :config
  (setq go-tag-args (list "-transform" "camelcase"))
  (with-eval-after-load 'go-ts-mode
    (define-key go-ts-mode-map (kbd "C-c t") #'go-tag-add)
    (define-key go-ts-mode-map (kbd "C-c T") #'go-tag-remove)))

(provide 'go)

;;; go.el ends here
