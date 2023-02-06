;;; go --- go configuration.

;;; Commentary:

;;; Functions:

(defun go/format-buffer-on-save ()
  "Format buffer before save. The depth of -10 places this before eglot's
   willSave notification, so that notifcation reports the actual contents
   that will be saved."
  (add-hook 'before-save-hook 'eglot-format-buffer -10 t))

;;; Hooks:

(add-hook 'go-ts-mode 'go/format-buffer-on-save)

;;; Code:

(provide 'go)

;;; go.el ends here
