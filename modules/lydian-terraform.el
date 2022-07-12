;;; lydian-terraform.el -- terraform configuration.

;;; Commentary:

;;; Code:

(use-package hcl-mode
    :config
    (hcl-mode))

(use-package terraform-mode
    :config
    (terraform-mode))

(use-package company-terraform)

(provide 'lydian-terraform)

;;; lydian-terraform.el ends here
