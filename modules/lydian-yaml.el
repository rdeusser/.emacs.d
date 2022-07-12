;;; lydian-yaml --- yaml-mode configuration.

;;; Commentary:

;;; Code:

;; yaml-mode doesn't derive from prog-mode, but we can at least enable
;; whitespace-mode and apply cleanup.
(use-package yaml-mode
    :config
    (add-hook 'yaml-mode-hook 'whitespace-mode)
    (add-hook 'yaml-mode-hook 'subword-mode))

(provide 'lydian-yaml)

;;; lydian-yaml.el ends here
