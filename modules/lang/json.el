;;; json --- json configuration.

;;; Commentary:

;;; Functions:

;;; Hooks:

;;; Code:

(defun jqfmt (start end)
  "Format the current selecton or buffer with jq."
  (interactive "r")
  (call-process-region start end "jq" t t t))

(defun jqfmt-minify (start end)
  "Format the current selection or buffer with jq and minify the result."
  (interactive "r")
  (call-process-region start end "jq" t t t "-c"))

(provide 'json)

;;; json.el ends here
