;;; lydian-ui.el -- UI config.

;;; Commentary:

;;; Code:

;; Paste over region.
(delete-selection-mode +1)

;; Font.
(set-frame-font "Source Code Pro" t)
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 100
                    :weight 'normal
                    :width 'normal)

;; (use-package doom-themes
;; 	:init
;; 	(setq doom-themes-enable-bold t)
;; 	(setq doom-themes-enable-italic t)
;; 	:config
;; 	(load-theme 'doom-peacock t)
;; 	(doom-themes-org-config))

;; (use-package gruber-darker-theme
;;   :straight t
;;   :config
;;   (load-theme 'gruber-darker t))

(use-package spacemacs-theme
  :straight t
  :defer t
  :init
  (custom-set-variables '(spacemacs-theme-custom-colors
                          '((bg . "#0a0814"))))
  (load-theme 'spacemacs-dark t))

;; (use-package kaolin-themes
;;   :straight t
;;   :config
;;   (setq kaolin-themes-bold t)
;;   (setq kaolin-themes-italic t)
;;   (setq kaolin-themes-underline t)
;;   (load-theme 'kaolin-aurora t)
;;   (set-background-color "#07090A"))

;; (use-package doom-themes
;;   :straight t
;;   :config
;;   (load-theme 'doom-outrun-electric t)
;;   ;; (setq doom-gruvbox-dark-variant "hard")
;;   (setq doom-themes-enable-bold t)
;;   (setq doom-themes-enable-italic t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)

;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; The toolbar is just a waste of valuable screen estate in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway.
(when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(menu-bar-mode -1)

;; The blinking cursor is nothing, but an annoyance.
(blink-cursor-mode -1)

;; Disable the annoying bell ring.
(setq ring-bell-function 'ignore)

;; Disable startup screen.
(setq inhibit-startup-screen t)

;; Nice scrolling.
(setq scroll-margin 0
    scroll-conservatively 100000
    scroll-preserve-screen-position 1)

;; Mode line settings.
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Enable y/n answers.
(fset 'yes-or-no-p 'y-or-n-p)

;; More useful frame title, that show either a file or a buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
    '("" invocation-name " " (:eval (if (buffer-file-name)
                                        (abbreviate-file-name (buffer-file-name))
                                        "%b"))))

(use-package smart-mode-line
    :init
    (setq sml/no-confirm-load-theme t)
    ;; delegate theming to the currently active theme
    (setq sml/theme nil)
    (add-hook 'after-hook #'sml/setup))

;; Show the cursor when moving after big movements in the window.
(use-package beacon
    :config
    (beacon-mode 1))

;; Show available keybindings after you start typing.
(use-package which-key
    :config
    (which-key-mode 1))

(provide 'lydian-ui)

;;; lydian-ui.el ends here
