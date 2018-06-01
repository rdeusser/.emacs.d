;;; lydian-rust.el -- Rust config.

;;; Commentary:

;;; Code:

(use-package rust-mode
	:config
	(rust-enable-format-on-save))

(use-package flycheck-rust)

(provide 'lydian-rust)

;;; lydian-rust.el ends here
