;;; lydian-ui.el -- UI config.

;;; Commentary:

;;; Code:

;; Source Code Pro ftw
(set-frame-font "Source Code Pro" t)

(use-package doom-themes
	:init
	(setq doom-themes-enable-bold t)
	(setq doom-themes-enable-italic t)
	:config
	(load-theme 'doom-peacock t)
	(doom-themes-org-config))

;; The toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(menu-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
    scroll-conservatively 100000
    scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers

(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
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

;; show the cursor when moving after big movements in the window
(use-package beacon
    :config
    (beacon-mode 1))

;; show available keybindings after you start typing
(use-package which-key
    :config
    (which-key-mode 1))

(provide 'lydian-ui)

;;; lydian-ui.el ends here
