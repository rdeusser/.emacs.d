;;; ui --- ui configuration.

;;; Commentary:

;;; Functions:

;;; Hooks:

;;; Code:

;; Set the default font.
(set-face-attribute 'default nil
                    :font "Source Code Pro"
                    :height 120
                    :weight 'regular)

;; Set the fixed pitch face.
(set-face-attribute 'fixed-pitch nil
                    :font "Source Code Pro"
                    :height 120
                    :weight 'regular)

;; Set the variable pitch face.
(set-face-attribute 'variable-pitch nil
                    :font "Source Code Pro"
                    :height 120
                    :weight 'regular)

;; Don't show splash screen.
(setq inhibit-splash-screen t)

;; Don't show startup screen.
(setq inhibit-startup-screen t)

;; Don't echo startup message.
(setq inhibit-startup-echo-area-message t)

;; Don't make that awful noise.
(setq ring-bell-function 'ignore)

;; Make Emacs start at fullscreen.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable y/n answers.
(fset 'yes-or-no-p 'y-or-n-p)

;; (use-package color-theme-sanityinc-tomorrow
;;   :config
;;   '(custom-enabled-themes '(sanityinc-tomorrow-night))
;;   (load-theme 'sanityinc-tomorrow-night t))

;; (use-package autothemer
;;   :config
;;   (load-theme 'catppuccin-mocha t))

;; (use-package doom-themes
;;   :init
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t)
;;   :config
;;   (load-theme 'doom-palenight t))

;; (use-package modus-themes
;;   :custom
;;   (modus-themes-italic-constructs t)
;;   (modus-themes-bold-constructs t)
;;   (modus-themes-mixed-fonts t)
;;   (modus-themes-headings '((1 1.3) (2 1.2) (3 1.1)))
;;   :init
;;   (setq modus-themes-mode-line '(borderless))
;;   (modus-themes-load-themes)
;;   :config
;;   (modus-themes-load-vivendi))

;; (use-package kaolin-themes
;;   :config
;;   (load-theme 'kaolin-bubblegum t))

(use-package gruber-darker-theme
  :config
  (defvar after-load-theme-hook nil
    "Hook to run after a color theme is loaded.")
  (defadvice load-theme (after run-after-load-theme-hook activate)
    "Run `after-load-theme-hook'."
    (run-hooks 'after-load-theme-hook))
  (defun customize-gruber-darker ()
    "Customize gruber-darker theme."
    (if (member 'gruber-darker custom-enabled-themes)
        (custom-set-faces
         '(font-lock-comment-face ((t (:foreground "#454545"))))
         '(font-lock-comment-delimface ((t (:foreground "#454545")))))))
  (add-hook 'after-load-theme-hook 'customize-gruber-darker)
  (load-theme 'gruber-darker t))

;; Mode line menu.
(use-package minions
  :config
  (minions-mode))

(use-package doom-modeline
  :after (all-the-icons all-the-icons-completion)
  :init
  ;; How tall the mode-line should be.
  (setq doom-modeline-height 25)

  ;; How wide the modeline bar should be.
  (setq doom-modeline-bar-width 4)

  ;; Should we use the hud instead of the default bar?
  (setq doom-modeline-hud 'nil)

  ;; The limit of the window width.
  (setq doom-modeline-window-width-limit 'nil)

  ;; How to detect the project root.
  ;; nil means to use `default-directory'.
  ;; The project management packages have some issues on detecting project root.
  ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
  ;; to hanle sub-projects.
  ;; You can specify one if you encounter the issue.
  (setq doom-modeline-project-detection 'auto)

  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   auto => emacs/l/comint.el (in a project) or comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are experiencing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)

  ;; Whether display icons in the mode-line.
  ;; While using the server mode in GUI, should set the value explicitly.
  (setq doom-modeline-icon t)

  ;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
  (setq doom-modeline-major-mode-icon t)

  ;; Whether display the colorful icon for `major-mode'.
  ;; It respects `all-the-icons-color-icons'.
  (setq doom-modeline-major-mode-color-icon t)

  ;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
  (setq doom-modeline-buffer-state-icon t)

  ;; Whether display the modification icon for the buffer.
  ;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
  (setq doom-modeline-buffer-modification-icon t)

  ;; Whether display the time icon. It respects variable `doom-modeline-icon'.
  (setq doom-modeline-time-icon t)

  ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
  (setq doom-modeline-unicode-fallback nil)

  ;; Whether display the buffer name.
  (setq doom-modeline-buffer-name t)

  ;; Whether display the minor modes in the mode-line.
  (setq doom-modeline-minor-modes t)

  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count t)

  ;; Major modes in which to display word count continuously.
  ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
  ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
  ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

  ;; Whether display the buffer encoding.
  (setq doom-modeline-buffer-encoding t)

  ;; Whether display the indentation information.
  (setq doom-modeline-indent-info nil)

  ;; If non-nil, only display one number for checker information if applicable.
  (setq doom-modeline-checker-simple-format t)

  ;; The maximum number displayed for notifications.
  (setq doom-modeline-number-limit 99)

  ;; The maximum displayed length of the branch name of version control.
  (setq doom-modeline-vcs-max-length 12)

  ;; Whether display the workspace name. Non-nil to display in the mode-line.
  (setq doom-modeline-workspace-name t)

  ;; Whether display the perspective name. Non-nil to display in the mode-line.
  (setq doom-modeline-persp-name nil)

  ;; If non nil the default perspective name is displayed in the mode-line.
  (setq doom-modeline-display-default-persp-name nil)

  ;; If non nil the perspective name is displayed alongside a folder icon.
  (setq doom-modeline-persp-icon nil)

  ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
  (setq doom-modeline-lsp nil)

  ;; Whether display the GitHub notifications. It requires `ghub' package.
  (setq doom-modeline-github nil)

  ;; The interval of checking GitHub.
  (setq doom-modeline-github-interval (* 30 60))

  ;; Whether display the modal state icon.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (setq doom-modeline-modal-icon nil)

  ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
  (setq doom-modeline-mu4e nil)

  ;; Whether display the gnus notifications.
  (setq doom-modeline-gnus t)

  ;; Whether gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
  (setq doom-modeline-gnus-timer 2)

  ;; Wheter groups should be excludede when gnus automatically being updated.
  (setq doom-modeline-gnus-excluded-groups '("dummy.group"))

  ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
  (setq doom-modeline-irc nil)

  ;; Function to stylize the irc buffer names.
  (setq doom-modeline-irc-stylize 'identity)

  ;; Whether display the time. It respects `display-time-mode'.
  (setq doom-modeline-time t)

  ;; Whether display the misc segment on all mode lines.
  ;; If nil, display only if the mode line is active.
  (setq doom-modeline-display-misc-in-all-mode-lines t)

  ;; Whether display the environment version.
  (setq doom-modeline-env-version nil)
  ;; Or for individual languages
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-ruby t)
  (setq doom-modeline-env-enable-perl t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-env-enable-elixir t)
  (setq doom-modeline-env-enable-rust t)

  ;; Change the executables to use for the language version string
  (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  (setq doom-modeline-env-ruby-executable "ruby")
  (setq doom-modeline-env-perl-executable "perl")
  (setq doom-modeline-env-go-executable "go")
  (setq doom-modeline-env-elixir-executable "iex")
  (setq doom-modeline-env-rust-executable "rustc")

  ;; What to display as the version while a new one is being loaded
  (setq doom-modeline-env-load-string "...")

  ;; Hooks that run before/after the modeline version string is updated
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil)
  :config
  (size-indication-mode)
  (column-number-mode)
  (doom-modeline-mode t))

(provide 'ui)

;;; ui.el ends here
