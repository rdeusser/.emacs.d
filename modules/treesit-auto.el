;;; treesit-auto --- treesit-auto configuration.

;;; Commentary:

;;; Functions:

;;; Hooks:

;;; Code:

(use-package treesit-auto
  :straight '(treesit-auto :type git
                           :host github
                           :repo "renzmann/treesit-auto"
                           :branch "main")
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;;; treesit-auto.el ends here
