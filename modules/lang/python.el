;;; python --- python configuration.

;;; Commentary:

;;; Functions:

(defun python/disable-clean-whitespace ()
  "Disables the xah-clean-whitepsace before-save-hook because
   it will nest functions under classes when I don't want them to be."
  (remove-hook 'before-save-hook 'xah-clean-whitespace))

;;; Hooks:

(add-hook 'python-ts-mode 'python/disable-clean-whitespace)

;;; Code:

(provide 'python)

;;; python.el ends here
