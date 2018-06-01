;;; lydian-git.el -- git config.

;;; Commentary:

;;; Code:

(use-package magit
	:config
	(global-set-key (kbd "C-x g") 'magit-status))

(provide 'lydian-git)

;;; lydian-git.el ends here
