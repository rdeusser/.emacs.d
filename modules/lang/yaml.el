;;; yaml --- yaml configuration.

;;; Commentary:

;;; Functions:

;;; Hooks:

;;; Code:

(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

(defun yqfmt (start end)
  "Format the current selecton or buffer with yq."
  (interactive "r")
  (setq current-point (point))
  (if (use-region-p)
      (call-process-region start end "yq" t t t)
    (call-process-region (point-min) (point-max) "yq" t t t))
  (goto-char current-point))

(provide 'yaml)

;;; yaml.el ends here
