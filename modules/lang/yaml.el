;;; yaml --- yaml configuration.

;;; Commentary:

;;; Functions:

;;; Hooks:

;;; Code:

(defun yqfmt (start end)
  "Format the current selecton or buffer with yq."
  (interactive "r")
  (call-process-region start end "yq" t t t))

(provide 'yaml)

;;; yaml.el ends here
