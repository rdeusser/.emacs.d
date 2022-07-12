;;; lydian-company.el --- company-mode setup

;;; Commentary:

;;; Code:

(use-package company
	:config
	(setq company-minimum-prefix-length 2)
	(global-company-mode 1))

(provide 'lydian-company)

;;; lydian-company.el ends here
