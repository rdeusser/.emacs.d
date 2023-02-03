(setq debug-on-error nil)

(defvar my/system-dir (file-name-directory load-file-name)
  "The root dir.")

(defvar my/core-dir (expand-file-name "core/" my/system-dir)
  "This directory houses all of the core functionality.")

(defvar my/modules-dir (expand-file-name  "modules/" my/system-dir)
  "This directory houses all of the built-in modules.")

(defvar my/savehist-dir (expand-file-name "savehist/" my/system-dir)
  "This folder stores all the automatically generated save/history-files.")

(defvar my/backup-dir (expand-file-name "backup/" my/system-dir)
  "This folder stores all the automatically generated backup files.")

(defvar my/themes-dir (expand-file-name "themes/" my/system-dir)
  "This folder stores all the users' themes.")

(unless (file-exists-p my/savehist-dir)
  (make-directory my/savehist-dir))

(unless (file-exists-p my/backup-dir)
  (make-directory my/backup-dir))

;; Add directories to Emacs's `load-path'.
(add-to-list 'load-path my/core-dir)
(add-to-list 'load-path my/modules-dir)
(add-to-list 'custom-theme-load-path my/themes-dir)

(require 'core)
(require 'editor)
(require 'ui)
(require 'modules)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" default)))
