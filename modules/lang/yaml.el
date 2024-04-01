;;; yaml --- yaml configuration.

;;; Commentary:

;;; Functions:

;;; Hooks:

;;; Code:

(use-package yaml-mode)

(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(defun yqfmt (start end)
  "Format the current selecton or buffer with yq."
  (interactive "r")
  (setq current-point (point))
  (if (use-region-p)
      (call-process-region start end "yq" t t t)
    (call-process-region (point-min) (point-max) "yq" t t t))
  (goto-char current-point))

(defun yqfmt-pretty (start end)
  "Format the current selecton or buffer with yq -P."
  (interactive "r")
  (setq current-point (point))
  (if (use-region-p)
      (call-process-region start end "yq" t t t)
    (call-process-region (point-min) (point-max) "yq" t t t "-P"))
  (goto-char current-point))

(provide 'yaml)

;;; yaml.el ends here
