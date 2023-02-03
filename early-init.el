;;; package.el:

(setq package-enable-at-startup nil)

;;; straight.el:

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(eval-when-compile
  (require 'use-package))

(when (eq system-type 'darwin)
  (let ((path-from-shell (shell-command-to-string
					      "/opt/homebrew/bin/fish --login -c 'string join : $PATH'"
						  )))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator)))
  (setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/12:/opt/homebrew/opt/libgccjit/lib/gcc/12:/opt/homebrew/opt/gcc/lib/gcc/12/gcc/aarch64-apple-darwin21/12"))
