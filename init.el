;; Uncomment to debug errors
;;(setq debug-on-error t)

;; Always initialize first
(require 'package)
(setq use-package-always-ensure t)
(package-initialize)
(add-to-list 'package-archives
	'("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(eval-when-compile
    (require 'use-package))

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar root-dir (file-name-directory load-file-name)
    "The root dir.")
(defvar core-dir (expand-file-name "core" root-dir)
    "This directory houses all of the core functionality.")
(defvar modules-dir (expand-file-name  "modules" root-dir)
    "This directory houses all of the built-in modules.")
(defvar savefile-dir (expand-file-name "savefile" root-dir)
    "This folder stores all the automatically generated save/history-files.")
(defvar themes-dir (expand-file-name "themes" root-dir)
    "This folder stores all the users' themes.")

(unless (file-exists-p savefile-dir)
    (make-directory savefile-dir))

;; add directories to Emacs's `load-path'
(add-to-list 'load-path core-dir)
(add-to-list 'load-path modules-dir)
(add-to-list 'custom-theme-load-path themes-dir)

(message "Loading core...")

(require 'lydian-core)
(require 'lydian-ui)
(require 'lydian-editor)
(require 'lydian-global-keybindings)
;; OSX specific settings
(when (eq system-type 'darwin)
    (require 'lydian-osx))

(message "Loading modules...")

(require 'lydian-emacs-lisp)
(require 'lydian-eshell)
(require 'lydian-ivy)
(require 'lydian-company)
(require 'lydian-go)
(require 'lydian-yaml)
(require 'lydian-c)
(require 'lydian-terraform)
(require 'lydian-git)
(require 'lydian-rust)
(require 'lydian-python)

;; auto-compile emacs lisp source code
(use-package auto-compile
    :config (auto-compile-on-load-mode)
    (auto-compile-on-save-mode))

(message "Ready")
