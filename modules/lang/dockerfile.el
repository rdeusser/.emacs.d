;;; dockerfile --- dockerfile configuration.

;;; Commentary:

;;; Functions:

(defun dockerfile/disable-aggressive-indent-mode ()
  "Disables aggressive indent mode because it's annoying in Dockerfiles."
  (aggressive-indent-mode 0))

;;; Hooks:

(add-hook 'dockerfile-ts-mode 'dockerfile/disable-aggressive-indent-mode)

;;; Code:

;;; Functions:

(provide 'dockerfile)

;;; dockerfile.el ends here
