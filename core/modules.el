;;; modules --- modules configuration.

;;; Commentary:

;;; Functions:

(defun require-all-files-in (directory)
  "Require all files in the directory DIRECTORY."
  (mapc (lambda (module)
          (load-file (concat directory "/" module))) (my/list-emacs-lisp-files directory)))

;;; Hooks:

;;; Code:

;; Language support.
(defvar my/lang-dir (expand-file-name "lang" my/modules-dir))

;; Add directories to Emacs's `load-path'.
(add-to-list 'load-path my/lang-dir)

;; Load all modules.
(require-all-files-in my/modules-dir)

;; Load all language modules.
(require-all-files-in my/lang-dir)

(provide 'modules)

;;; modules.el ends here
