;; Uncomment to debug errors
;;(setq debug-on-error t)

(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'use-package)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Always load newest byte code.
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

;; Add directories to Emacs's `load-path'.
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

(require 'lydian-bazel)
(require 'lydian-c)
(require 'lydian-company)
(require 'lydian-dockerfile)
(require 'lydian-emacs-lisp)
(require 'lydian-eshell)
(require 'lydian-git)
(require 'lydian-go)
(require 'lydian-ivy)
(require 'lydian-json)
(require 'lydian-lsp)
(require 'lydian-markdown)
(require 'lydian-protobuf)
(require 'lydian-python)
(require 'lydian-rust)
(require 'lydian-terraform)
(require 'lydian-toml)
(require 'lydian-typescript)
(require 'lydian-yaml)

;; auto-compile emacs lisp source code
(use-package auto-compile
    :config (auto-compile-on-load-mode)
    (auto-compile-on-save-mode))

(message "Ready")
