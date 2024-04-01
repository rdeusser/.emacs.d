;;; ui --- ui configuration.

;;; Commentary:

;;; Functions:

;;; Hooks:

;;; Code:

;; Set the default font.
(set-face-attribute 'default nil
                    :font "JetBrains Mono"
                    :height 120
                    :weight 'regular)

;; Set the fixed pitch face.
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono"
                    :height 120
                    :weight 'regular)

;; Set the variable pitch face.
(set-face-attribute 'variable-pitch nil
                    :font "JetBrains Mono"
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

;; Disable toolbar.
(setq tool-bar-mode nil)

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
  (advice-add load-theme (after run-after-load-theme-hook activate)
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

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :init
  (all-the-icons-completion-mode))

(use-package kind-icon
  :if (display-graphic-p)
  :custom
  (kind-icon-use-icons t)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  ;; Add hook to reset cache so the icon colors match my theme.
  (add-hook 'my/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

(use-package simple-modeline
  :init
  (defun simple-modeline-segment-line-count ()
    "Display the line count in the mode-line."
    (if (use-region-p)
        (format " %d Lines" (count-lines (region-beginning) (region-end)))))
  :config
  (setq simple-modeline-segments
        '((simple-modeline-segment-modified
           simple-modeline-segment-buffer-name
           simple-modeline-segment-position
           simple-modeline-segment-line-count)
          (simple-modeline-segment-minor-modes
           simple-modeline-segment-input-method
           simple-modeline-segment-eol
           simple-modeline-segment-encoding
           simple-modeline-segment-vc
           simple-modeline-segment-misc-info
           simple-modeline-segment-process
           simple-modeline-segment-major-mode)))
  :hook (after-init . simple-modeline-mode))

(provide 'ui)

;;; ui.el ends here
