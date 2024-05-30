;;; treesit-auto --- treesit-auto configuration.

;;; Commentary:

;;; Functions:

;;; Hooks:

;;; Code:

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; treesit-auto.el ends here
