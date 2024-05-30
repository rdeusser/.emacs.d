;;; modules --- modules configuration.

;;; Commentary:

;;; Functions:

(defun require-all-files-in (directory)
  "Require all files in the directory DIRECTORY."
  (mapc (lambda (module)
          (load-file (concat directory "/" module))) (my/list-emacs-lisp-files directory)))

;;; Hooks:

;;; Code:

;; Add directories to Emacs's `load-path'.
(add-to-list 'load-path my/modules-dir)

;; Load all modules.
(require-all-files-in my/modules-dir)

(provide 'modules)

;;; modules.el ends here
