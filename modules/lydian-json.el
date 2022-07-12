;;; lydian-json --- json-mode configuration.

;;; Commentary:

;;; Code:

(use-package json-mode
  :config
  (defun jqfmt ()
    "Format the current file with jq and reload the buffer"
    (interactive)
    (shell-command
     (format "jq '.' %s | sponge %s"
             (shell-quote-argument (buffer-file-name))
	         (shell-quote-argument (buffer-file-name))))
    (revert-buffer t t t)))

(provide 'lydian-json)

;;; lydian-json.el ends here
