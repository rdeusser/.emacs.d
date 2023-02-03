;;; core --- core configuration.

;;; Commentary:

;;; Functions:

(defun my/clear-savehist-dir ()
  (interactive)
  (delete-directory my/savehist-dir t t)
  (make-directory my/savehist-dir))

(defun my/clear-backup-dir ()
  (interactive)
  (delete-directory my/backup-dir t t)
  (make-directory my/backup-dir))

(defun my/list-emacs-lisp-files (directory)
  (directory-files directory nil "\\.el$"))

(defun my/reload-configuration ()
  "Reload configuration."
  (interactive)
  (load-file (concat my/system-dir "init.el"))
  (mapc (lambda (module)
          (load-file (concat my/core-dir module))) (my/list-emacs-lisp-files my/core-dir))
  (smartparens-global-mode t))

;;; Hooks:

;; Make a shell script executable automatically on save.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; Code:

;; I don't use tramp.
(setq tramp-mode nil)

;; Saner regex syntax.
(setq reb-re-syntax 'string)

;; MacOS specific settings.
(when (eq system-type 'darwin)
  (use-package osx-trash
    :init
    (setq delete-by-moving-to-trash t)
    :config
    (osx-trash-setup)))

;; Backup directory config.
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,my/backup-dir)))
(setq delete-old-versions t)
(setq kept-new-versions 12)
(setq kept-old-versions 6)
(setq version-control t)

;; Auto save config.
(setq auto-save-file-name-transforms `((".*" ,my/savehist-dir t)))

(provide 'core)

;;; core.el ends here
