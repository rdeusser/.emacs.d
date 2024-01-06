;;; json --- json configuration.

;;; Commentary:

;;; Functions:

(defun jqfmt (start end)
  "Format the current selecton or buffer with jq."
  (interactive "r")
  (if (use-region-p)
      (call-process-region start end "jq" t t t)
    (call-process-region (point-min) (point-max) "jq" t t t)))

(defun jqfmt (start end)
  "Format the current selection or buffer with jq and minify the result."
  (interactive "r")
  (if (use-region-p)
      (call-process-region start end "jq" t t t)
    (call-process-region (point-min) (point-max) "jq" t t t "-c")))

;;; Hooks:

;;; Code:

(provide 'json)

;;; json.el ends here
