;;; json --- json configuration.

;;; Commentary:

;;; Functions:

;;; Hooks:

;;; Code:

(defun jqfmt (start end)
  "Format the current selecton or buffer with jq."
  (interactive "r")
  (call-process-region start end "jq" t t))

(provide 'json)

;;; json.el ends here
