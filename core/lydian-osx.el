;;; lydian-osx.el -- macOS configuration.

;;; Commentary:

;;; Code:

;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH"))

;; It's all in the Meta.
(setq ns-function-modifier 'hyper)

;; proced-mode doesn't work on macOS so we use vkill instead.
(use-package vkill
  :config
  (autoload 'vkill "vkill" nil t)
  (global-set-key (kbd "C-x p") 'vkill))

(menu-bar-mode t)

(use-package ns-auto-titlebar
  :config
  (ns-auto-titlebar-mode))

;; Enable emoji, and stop the UI from freezing when trying to display them.
(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(use-package osx-trash-setup
  :config
  (osx-trash-setup t)
  (setq delete-by-moving-to-trash t))

(provide 'lydian-osx)

;;; lydian-osx.el ends here
