;;; lydian-eshell.el -- eshell config.

;;; Commentary:

;;; Code:

(use-package xterm-color
	:init
	(setq xterm-color-preserve-properties t))

(defun lydian/setup-eshell ()
	(interactive)
	;; turn off semantic mode in eshell buffers
	(semantic-mode -1)
	(local-set-key (kbd "M-P") 'eshell-previous-prompt)
	(local-set-key (kbd "M-N") 'eshell-next-prompt)
	(local-set-key (kbd "M-R") 'eshell-previous-matching-input))

(use-package eshell
	:init
	(setq eshell-prompt-regexp "⚡ "
		eshell-prompt-function 'eshell-prompt
		eshell-glob-case-insensitive nil
		eshell-error-if-no-glob nil
		eshell-scroll-to-bottom-on-input nil
		eshell-where-to-jump 'begin
		eshell-review-quick-commands nil
		eshell-smart-space-goes-to-end t
		eshell-cmpl-cycle-completions nil
		eshell-buffer-maximum-lines 100000
		eshell-history-size 500
		eshell-buffer-shorthand t
		eshell-highlight-prompt nil
		eshell-plain-echo-behavior t
		eshell-ls-initial-args "-lh")
	:config
	(require 'em-smart)
	(require 'esh-opt)
	(require 'em-cmpl)
	(require 'em-prompt)
	(require 'em-term)

	(setenv "PAGER" "cat")
	(setenv "TERM" "xterm-256color")
	(setenv "GOPATH" "~/go")

	(defalias 'emacs 'find-file)

	(setq eshell-visual-commands
		'("bower" "htop" "npm" "tail" "top" "vim" "watch" "yarn" "ssh" "rustup"))
	(setq eshell-visual-subcommands
		'(("docker" "build")
			 ("git" "log" "diff" "show")
			 ("npm" "init" "install")
			 ("yarn" "init" "install")
			 ("vagrant" "ssh")))

	(defun lydian/create-all-eshell-buffers ()
		(interactive)
		;; sprocket
		(let ((eshell-buffer-name "*shell-sprocket*")
				 (default-directory "~/go/src/github.exacttarget.com/cloud-fabric/sprocket"))
			(eshell))
		;; fabric
		(let ((eshell-buffer-name "*shell-fabric*")
				 (default-directory "~/src/fabric"))
			(eshell)))

	(defun lydian/truncate-eshell-buffers ()
		"Truncates all eshell buffers"
		(interactive)
		(save-current-buffer
			(dolist (buffer (buffer-list t))
				(set-buffer buffer)
				(when (eq major-mode 'eshell-mode)
					(eshell-truncate-buffer)))))

	;; After being idle for 10 seconds, truncate all the
	;; eshell-buffers if needed.
	(setq lydian/eshell-truncate-timer
		(run-with-idle-timer 10 t #'lydian/truncate-eshell-buffers))

	(defun lydian/reset-colors ()
		"Reset shell colors."
		(interactive)
		(end-of-buffer)
		(insert "echo -e \"\033[m\"")
		(eshell-send-input nil t))

	(defun eshell/cds ()
		"Change directory to the project's root."
		(eshell/cd (locate-dominating-file default-directory ".git")))

	(defalias 'eshell/l 'eshell/ls)
	(defalias 'eshell/ll 'eshell/ls)

	(defun eshell/ec (pattern)
		(if (stringp pattern)
			(find-file pattern)
			(mapc #'find-file (mapcar #'expand-file-name pattern))))
	(defalias 'e 'eshell/ec)
	(defalias 'ee 'find-file-other-window)

	(defun eshell/d (&rest args)
		(dired (pop args) "."))

	(defun eshell/clear ()
		"Clear the eshell buffer"
		(interactive)
		(let ((eshell-buffer-maximum-lines 0))
			(eshell-truncate-buffer)
			(let ((inhibit-read-only t))
				(erase-buffer))))

	(defun eshell/icat (&rest args)
		"Display image(s)."
		(let ((elems (eshell-flatten-list args)))
			(while elems
				(eshell-printn
					(propertize " "
						'display (create-image (expand-file-name (car elems)))))
				(setq elems (cdr elems))))
		nil)

	(defun eshell/magit ()
		"Function to open magit-status for the current directory"
		(interactive)
		(magit-status (locate-dominating-file default-directory ".git"))
		nil)

	(add-hook 'eshell-mode-hook #'lydian/setup-eshell)

	;; So the history vars are defined
	(require 'em-hist)
	(if (boundp 'eshell-save-history-on-exit)
		;; Don't ask, just save
		(setq eshell-save-history-on-exit t))

	(defun eshell-prompt ()
		"An eshell prompt for lydian."
		(interactive)
		(let* ((pwd (eshell/pwd))
				  (username (user-login-name))
				  (directory (split-directory (pwd-shorten-dirs (pwd-replace-home pwd))))
				  (parent (car directory))
				  (name   (cadr directory))
				  (branch (curr-dir-git-branch-string pwd))
				  (user-face `(:inherit font-lock-builtin-face))
				  (dir-face `(:inherit font-lock-keyword-face))
				  (git-face `(:inherit font-lock-function-name-face :slant italic)))
			(concat (propertize username 'face user-face)
				" in "
				(propertize parent 'face dir-face)
				(propertize name 'face dir-face)
				(when branch
					(concat
						" on "
						(propertize branch 'face git-face)))
				(propertize "\n⚡ "))))

	(defun curr-dir-git-branch-string (pwd)
		"Return current git branch as a string, or the empty string if PWD is not in a git repo (or the git command is not found)."
		(interactive)
		(when (and (not (file-remote-p pwd))
				  (eshell-search-path "git")
				  (locate-dominating-file pwd ".git"))
			(let* ((git-url (shell-command-to-string "git config --get remote.origin.url"))
					  (git-output (shell-command-to-string (concat "git rev-parse --abbrev-ref HEAD")))
					  (git-branch (s-trim git-output)))
				git-branch)))

	(defun pwd-replace-home (pwd)
		"Replace home in PWD with tilde (~) character."
		(interactive)
		(let* ((home (expand-file-name (getenv "HOME")))
				  (home-len (length home)))
			(if (and (>= (length pwd) home-len)
					(equal home (substring pwd 0 home-len)))
				(concat "~" (substring pwd home-len)) pwd)))

	(defun pwd-shorten-dirs (pwd)
		"Shorten all directory names in PWD except the last two."
		(let ((p-lst (split-string pwd "/")))
			(if (> (length p-lst) 2)
				(concat (mapconcat (lambda (elm)
									   (if (zerop (length elm)) "" (substring elm 0 1)))
							(butlast p-lst 2) "/") "/" (mapconcat (lambda (elm) elm)
														   (last p-lst 2) "/")) pwd))) ;; Otherwise, we just return the PWD

	(defun split-directory (directory)
		(if (string-match-p ".*/.*" directory)
			(list (file-name-directory directory)
				(file-name-base directory))
			(list "" directory)))

	(defun eshell-here ()
		"Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
		(interactive)
		(let* ((parent (if (buffer-file-name)
						   (file-name-directory (buffer-file-name))
						   default-directory))
				  (name   (car (last (split-string parent "/" t)))))
			(eshell "new")
			(rename-buffer (concat "*shell-" name "*"))
			(erase-buffer)
			(eshell-send-input)))

	(defun eshell-there (host)
		(interactive "sHost: ")
		(let ((default-directory (format "/%s:" host)))
			(eshell host))))

(provide 'lydian-eshell)

;;; lydian-eshell.el ends here
