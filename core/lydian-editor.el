;;; lydian-editor.el -- editor config.

;;; Commentary:

;;; Code:

(scroll-bar-mode nil)
(tool-bar-mode   nil)
(tooltip-mode    nil)
(menu-bar-mode   nil)
(column-number-mode t)

;; Set fill-column to 80 by default (for comments only).
(setq-default fill-column 80)

(defun fill-paragraph-code-only (beg end)
  (interactive "*r")
  (let ((fill-column 120))
	(fill-region beg end)
	(setq fill-column 80)))

(define-key global-map "\M-q" 'fill-paragraph-code-only)

;; Set auto-fill mode on in all major modes.
(auto-fill-mode t)
(setq-default auto-fill-function 'do-auto-fill)

;; But only on comments.
(setq comment-auto-fill-only-comments t)

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single logical line."
  (interactive "*r")
  (let ((fill-column (point-max)))
	(fill-region beg end)))

(defun rewrap-region (beg end)
  "Rewraps the region by calling unfill-region, adding a space, and then deleting to cause auto-fill-mode to kick in."
  (interactive "*r")
  (unfill-region beg end)
  (do-auto-fill))

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
(setq find-file-suppress-same-file-warnings t)

(defun lydian-prog-mode-defaults ()
  (define-key prog-mode-map (kbd "M-(") (wrap-with "("))
  (define-key prog-mode-map (kbd "M-[") (wrap-with "["))
  (define-key prog-mode-map (kbd "M-\"") (wrap-with "\""))
  (smartparens-global-mode t))

(setq lydian-prog-mode-hook 'lydian-prog-mode-defaults)

(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks 'lydian-prog-mode-hook)))

(setq-default indent-tabs-mode nil) ;; don't use tabs to indent
(setq-default tab-width 4)          ;; but maintain appearance

;; Only indent the line when at BOL or in a line's indentation. Anywhere else,
;; insert literal indentation.
(setq-default tab-always-indent nil)

;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
;; middle of a line.
(setq tabify-regexp "^\t* [ \t]+")

;; Continue wrapped words at whitespace, rather than in the middle of a word.
(setq-default word-wrap t)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

(setq require-final-newline t)

(add-hook 'text-mode-hook #'visual-line-mode)

(setq kill-do-not-save-duplicates t)

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode t))

;; Newline at end of file.
(setq require-final-newline t)

;; Delete the selection with a keypress.
(delete-selection-mode t)

;; Store all backup and autosave files in the tmp dir.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Autosave the undo-tree history.
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-diff nil)
  (global-undo-tree-mode t))

;; Revert buffers automatically when underlying files are changed externally.
(global-auto-revert-mode t)

;; hippie expand is dabbrev expand on steroids.
(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers
															try-expand-dabbrev-from-kill
															try-complete-file-name-partially try-complete-file-name
															try-expand-all-abbrevs try-expand-list try-expand-line
															try-complete-lisp-symbol-partially
															try-complete-lisp-symbol))

;; Smart tab behavior - indent or complete.
(setq tab-always-indent 'complete)

(use-package smartparens
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode t)
  (smartparens-global-strict-mode t))

;; Disable annoying blink-matching-paren.
(setq blink-matching-paren nil)

;; Enable showing matching parens.
(show-paren-mode t)

;; Electric pair - automatically insert matching delimiters.
;; (electric-pair-mode t)

;; Meaningful names for buffers with the same name.
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; Saveplace remembers your location in a file when saving files.
(setq save-place-file (expand-file-name "saveplace" savefile-dir))

;; Savehist keeps track of some history.
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" savefile-dir))
(savehist-mode t)

(defun recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (-any-p (lambda (dir)
              (string-prefix-p dir file-dir))
            (mapcar 'file-truename (list savefile-dir package-user-dir)))))

;; Save recent files.
(use-package recentf
  :init (setq recentf-save-file (expand-file-name "recentf" savefile-dir) recentf-max-saved-items
              500 recentf-max-menu-items 15
              ;; disable recentf-cleanup on Emacs start, because it can cause
              ;; problems with remote files
              recentf-auto-cleanup 'never)
  :config (add-to-list 'recentf-exclude 'recentf-exclude-p)
  (recentf-mode t))

;; Use shift + arrow keys to switch between visible buffers
(windmove-default-keybindings)

(defvar auto-save t)

;; Automatically save buffers associated with files on buffer switch and on windows switch.
(defun auto-save-command ()
  "Save the current buffer if `auto-save' is not nil."
  (when (and auto-save
             buffer-file-name
             (buffer-modified-p (current-buffer))
             (file-writable-p buffer-file-name))
    (save-buffer)))

(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
The body of the advice is in BODY."
  `(progn ,@(mapcar (lambda (command)
                      `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-"
                                                                    advice-name)) activate)
                         ,@body)) commands)))

;; Advise all window switching functions.
(advise-commands "auto-save" (switch-to-buffer other-window windmove-up windmove-down windmove-left
											   windmove-right) before (auto-save-command))

(add-hook 'mouse-leave-buffer-hook 'auto-save-command)

(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (when (and mode
               (consp mode))
      (setq mode (car mode)))
    (with-current-buffer buffer (if mode (funcall mode)))))

;; Highlight the current line.
(global-hl-line-mode t)

(use-package volatile-highlights
  :diminish volatile-highlights-mode)
:config
(volatile-highlights-mode t)

;; Note: this should be after volatile-highlights. It is required add the ability to cut the current line, without
;; marking it.
(use-package crux
  :config (crux-with-region-or-line kill-region))

;; Tramp, for sudo access.
(setq tramp-default-method "ssh")

(set-default 'imenu-auto-rescan t)

;; Enable narrowing commands.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Enabled change region case commands.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Enable erase-buffer command.
(put 'erase-buffer 'disabled nil)

;; Bookmarks.
(setq bookmark-default-file (expand-file-name "bookmarks" savefile-dir) bookmark-save-flag 1)

;; Projectile is a project management mode.
(use-package projectile
  :init (setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir))
  :config (projectile-mode t))

;; Avy allows us to effectively navigate to visible things.
(setq avy-background t)
(setq avy-style 'at-full)

;; anzu-mode enhances isearch & query-replace by showing total matches and current match position.
(use-package anzu
  :init (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
  :diminish anzu-mode
  :config (global-anzu-mode 1))

;; Dired - reuse current buffer by pressing 'a'.
(put 'dired-find-alternate-file 'disabled nil)

;; Always delete and copy recursively.
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; If there is a dired buffer displayed in the next window, use its current subdir, instead of the current subdir of
;; this dired buffer.
(setq dired-dwim-target t)

;; Enable some really cool extensions like C-x C-j (dired-jump)

;; ediff - don't start another frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Clean up obsolete buffers automatically.

;;Ssmarter kill-ring navigation.
(use-package browse-kill-ring
  :init (global-set-key (kbd "s-y") 'browse-kill-ring)
  :config (browse-kill-ring-default-keybindings))

(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive (list (not (region-active-p)))))

(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive (if mark-active (list (region-beginning)
                                        (region-end))
                    (list (point-min)
                          (point-max))))))

(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

;; Automatically indenting yanked text if in programming-modes.
(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) 1000)
      (indent-region beg end nil)))

(defvar yank-indent-modes '(LaTeX-mode TeX-mode))
(defvar indent-sensitive-modes '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode))

(advise-commands "indent" (yank yank-pop) after "If current mode is one of `yank-indent-modes',
indent yanked text (with prefix arg don't indent)." (if (and (not (ad-get-arg 0))
                                                             (not (member major-mode
																		  indent-sensitive-modes))
                                                             (or (derived-mode-p 'prog-mode)
                                                                 (member major-mode
																		 yank-indent-modes)))
                                                        (let ((transient-mark-mode nil))
                                                          (yank-advised-indent-function
                                                           (region-beginning)
                                                           (region-end)))))

;; abbrev config.
(add-hook 'text-mode-hook 'abbrev-mode)

;; Make a shell script executable automatically on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; whitespace-mode config.
(setq whitespace-line-column 120) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; Saner regex syntax.
(setq reb-re-syntax 'string)

(setq eshell-directory-name (expand-file-name "eshell" savefile-dir))

(setq semanticdb-default-save-directory (expand-file-name "semanticdb" savefile-dir))

;; Compilation from Emacs.
(defun colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min)
								  (point-max)))))

(setq compilation-ask-about-save nil)
(setq compilation-always-kill t)
(setq compilation-scroll-output 'first-error)

;; Colorize output of Compilation Mode, see http://stackoverflow.com/a/3072831/355252.
(add-hook 'compilation-filter-hook #'colorize-compilation-buffer)

;; Enable winner-mode to manage window configurations.
(winner-mode t)

;; easy-kill
(use-package easy-kill
  :config (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

;; operate-on-number
(use-package smartrep
  :config
  (smartrep-define-key global-map "C-c ." '(("+" . apply-operation-to-number-at-point)
                                            ("-" . apply-operation-to-number-at-point)
                                            ("*" . apply-operation-to-number-at-point)
                                            ("/" . apply-operation-to-number-at-point)
                                            ("\\" . apply-operation-to-number-at-point)
                                            ("^" . apply-operation-to-number-at-point)
                                            ("<" . apply-operation-to-number-at-point)
                                            (">" . apply-operation-to-number-at-point)
                                            ("#" . apply-operation-to-number-at-point)
                                            ("%" . apply-operation-to-number-at-point)
                                            ("'" . operate-on-number-at-point))))

;; Use settings from .editorconfig file when present.
(use-package editorconfig
  :config (editorconfig-mode t))

(use-package with-editor
  :init
  (progn
	(add-hook 'shell-mode-hook  'with-editor-export-editor)
	(add-hook 'eshell-mode-hook 'with-editor-export-editor)))

;; Multiple cursors (multiedit).
(use-package multiple-cursors)

;; Create missing directories when we open a file that doesn't exist under a
;; directory tree that may not exist.
(add-hook! 'find-file-not-found-functions
  (defun doom-create-missing-directories-h ()
    "Automatically create missing directories when creating new files."
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                               parent-directory))
             (progn (make-directory parent-directory 'parents)
                    t))))))

;; Less aggressive `delete-trailing-whitespaces' on save.
(use-package ws-butler
  :hook (doom-first-buffer . ws-butler-global-mode)
  :config
  ;; ws-butler normally preserves whitespace in the buffer (but strips it fromthe written file). While sometimes convenient, this behavior is not intuitive. To the average user it looks like whitespace cleanup is failing, which causes folks to redundantly install their own.
  (setq ws-butler-keep-whitespace-before-point nil))

;; Show ANSI colors.
(use-package ansi-color
  :config
  (defun display-ansi-colors ()
    (ansi-color-apply-on-region (point-min) (point-max)))
  (add-hook 'text-mode-hook 'display-ansi-colors))

(provide 'lydian-editor)

;;; lydian-editor.el ends here
