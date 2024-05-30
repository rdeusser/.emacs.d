;;; magit --- magit configuration.

;;; Commentary:

;;; Functions:

;;; Hooks:

;;; Code:

(use-package magit
  :init
  (transient-define-suffix magit-open-pull-request ()
    "Open the pull request URL."
    (interactive)
    (save-excursion
      (set-buffer (magit-process-buffer t))
      (goto-char (point-max))
      (magit-section-backward)
      (when
          (search-backward-regexp "remote: \\(To create a merge\\|Create pull\\) request" nil t)
        (forward-line 1)
        (re-search-forward "remote: +" (line-end-position) t)
        (browse-url-at-point))))
  (transient-append-suffix 'magit-push "p"
    '("O" "Open pull request" magit-open-pull-request)))

(use-package magit-section)
(use-package compat)

(provide 'magit)

;;; magit.el ends here
