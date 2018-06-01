;;; lydian-c.el -- c-mode config.

;;; Commentary:

;;; Code:

(defun lydian-c-mode-defaults ()
    "Defaults for 'c-mode'."
    (setq c-default-style "k&r")
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0))

(setq lydian-c-mode-hook 'lydian-c-mode-defaults)

;; this will affect all modes derived from c-mode, like
;; java-mode, php-mode, etc
(add-hook 'c-mode-hook (lambda ()
                           (run-hooks 'lydian-c-mode-hook)))

(defun lydian-makefile-mode-defaults ()
    "Defaults for 'makefile-mode'."
    (whitespace-toggle-options '(tabs))
    (setq indent-tabs-mode t))

(setq lydian-makefile-mode-hook 'lydian-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda ()
                                  (run-hooks 'lydian-makefile-mode-hook)))
(provide 'lydian-c)

;;; lydian-c.el ends here
