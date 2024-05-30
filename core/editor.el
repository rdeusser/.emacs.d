;;; editor --- editor configuration.

;;; Commentary:

;;; Functions:

(defun xah-clean-whitespace ()
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or text selection, respects `narrow-to-region'.

URL `http://xahlee.info/emacs/emacs/elisp_compact_empty_lines.html'
Version 2017-09-22 2020-09-08"
  (interactive)
  (let ($begin $end)
    (if (use-region-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+\n" nil "move")
            (replace-match "\n")))
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil "move")
            (replace-match "\n\n")))
        (progn
          (goto-char (point-max))
          (while (equal (char-before) 32) ; char 32 is space
            (delete-char -1))))
      (message "Whitespace cleaned"))))

(defun unfill-region (start end)
  "Unfill the region, joining text paragraphs into a single logical line (args: START END)."
  (interactive "r*")
  (let ((fill-column (point-max)))
    (fill-region start end)))

(defun rewrap-region (start end)
  (interactive "r*")
  (unfill-region start end)
  (do-auto-fill))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun delete-region-or-char (start end)
  "Deletes the active region or the char if region is not active (args: START END)."
  (interactive "r*")
  (transient-mark-mode nil)
  (if (use-region-p)
      (puni-delete-active-region start end)
    (puni-backward-delete-char))
  (transient-mark-mode))

(defun indent-between-pair (&rest _ignored)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

(defun my/base64-encode-region (old-fn BEG END &optional NO-LINE-BREAK)
  "Wrapper around the builtin 'base64-encode-region' that base64 encodes the region without any line breaks."
  (funcall old-fn (mark) (point) t))

(advice-add 'base64-encode-region :around #'my/base64-encode-region)

(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun format-buffer-with (command &rest args)
  "Formats the current buffer with COMMAND with optional ARGS."
  (setq current-point (point))
  (let ((temp-buffer (generate-new-buffer " *temp*"))
        (error-buffer (get-buffer-create (format "*%s-output*" command)))
        (current-buffer (current-buffer)))
    (with-current-buffer temp-buffer
      (insert (buffer-string)))
    (let ((exit-code (apply #'call-process-region (point-min) (point-max) command nil `(,temp-buffer t) nil args)))
      (if (= exit-code 0)
          (progn
            (with-current-buffer current-buffer
              (when (get-buffer error-buffer)
                (quit-windows-on error-buffer))
              (replace-buffer-contents temp-buffer))
            (message (format "Buffer formatted with \"%s\"" (string-trim (format "%s %s" command (string-join args " "))))))
        (progn
          (with-current-buffer error-buffer
            (read-only-mode 0)
            (insert-buffer temp-buffer)
            (special-mode)
            (display-buffer error-buffer))
          (error "%s formatting failed with exit code %s" command exit-code))))
    (kill-buffer temp-buffer))
  (goto-char current-point))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

(defun add-spaces-around-braces ()
  "Ensure there is a space after '{{' and a space before '}}' in the buffer."
  (interactive)
  ;; Ensure space after {{
  (goto-char (point-min))  ; Start at the beginning of the buffer
  (while (re-search-forward "\\({{\\)\\([^ ]\\)" nil t)
    (replace-match "\\1 \\2" nil nil))

  ;; Ensure space before }}
  (goto-char (point-min))  ; Start again at the beginning of the buffer
  (while (re-search-forward "\\([^ ]\\)\\(}}\\)" nil t)
    (replace-match "\\1 \\2" nil nil)))

;;; Hooks:

(add-hook 'before-save-hook #'xah-clean-whitespace)

;;; Code:

;; Disable bars.
(scroll-bar-mode    nil)
(tool-bar-mode      nil)
(menu-bar-mode      nil)
(column-number-mode)

;; Set default tab width.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Set fill column to 100 by default (for comments only).
(setq-default fill-column 100)

;; Allows word wrapping.
(setq-default word-wrap t)

;; Set auto-fill mode on in all major modes.
(auto-fill-mode nil) ;; turn off for now
(setq-default auto-fill-function 'do-auto-fill)

;; But only on comments.
(setq comment-auto-fill-only-comments t)

;; Continue wrapped words at whitespace, rather than in the middle of the word.
(setq-default word-wrap t)
(setq-default truncate-lines t)

;; Enable writing over the active region.
(delete-selection-mode)

;; Highlight the current line.
(global-hl-line-mode)

;; Show line numbers globally.
;; (global-display-line-numbers-mode)

;; Keybindings.
(global-unset-key (kbd "C-x C-c")) ;; I always hit these keys by mistake
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; Revert buffers automatically when underlying files are changed externally.
(global-auto-revert-mode)

(use-package caser)

(use-package crux
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
	     ([(shift return)] . crux-smart-open-line)
	     ("C-DEL" . crux-kill-line-backwards)
	     ("C-c n" . crux-cleanup-buffer-or-region)
	     ("C-c e" . crux-eval-and-replace)
	     ("C-c D" . delete-file-and-buffer)
	     ("C-c r" . crux-rename-file-and-buffer))
  :config
  (crux-with-region-or-line kill-region)
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify))

(use-package ws-butler
  :init
  ;; ws-butler normally preserves whitespace in the buffer (but strips it
  ;; from the written file). While sometimes convenient, this behavior is not
  ;; intuitive. To the average user it looks like whitespace cleanup is failing,
  ;; which causes folks to redundantly install their own.
  (setq ws-butler-keep-whitespace-before-point nil)
  :config
  (ws-butler-global-mode))

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package puni
  :bind (("C-c DEL" . puni-force-delete)
         ("C-d" . puni-forward-delete-char)
         ("DEL" . puni-backward-delete-char)
         ("M-d" . puni-forward-kill-word)
         ("M-DEL" . puni-backward-kill-word)
         ("C-k" . puni-kill-line)
         ("C-S-k" . puni-backward-kill-line)
         ("C-M-n" . puni-forward-sexp)
         ("C-M-p" . puni-backward-sexp))
  :config
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  (puni-global-mode)
  (electric-pair-mode))

(use-package aggressive-indent
  :init
  (setq aggressive-indent-protected-commands '(undo undo-tree-undo undo-tree-redo undo-tree-visualize undo-tree-visualize-undo undo-tree-visualize-redo whitespace-cleanup))
  (global-aggressive-indent-mode))

;; Persist history over Emacs restarts.
(use-package savehist
  :init
  (savehist-mode))

(use-package recentf
  :init
  (recentf-mode)
  :config
  (run-at-time nil 90 'recentf-save-list))

;; Visualize undo history as a tree.
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-diff nil)
  (setq undo-tree-enable-undo-in-region nil)
  (setq undo-tree-visualizer-lazy-drawing 50)
  (setq undo-tree-history-directory-alist `((".*" . ,my/savehist-dir)))
  :config
  (defun prune-undo-tree ()
    (interactive)
    (setq buffer-undo-tree nil))
  (global-undo-tree-mode))

;;; LSP

(use-package eglot
  :bind (("C-c l e" . eglot)
         ("C-c l a" . eglot-code-actions)
         ("C-c l r" . eglot-rename)))

;;; Completion/minibuffer section

(use-package avy
  :bind (("C-c j" . avy-goto-line)
         ("s-j" . avy-goto-char-timer)))

(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-count 10)
  (setq vertico-cycle t))

(use-package vertico-directory
  ;; More convenient directory navigation commands
  :straight nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-word)
              ("M-DEL" . vertico-directory-delete-char))

  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

                                        ; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-s" . consult-line)
         ("C-c s" . consult-line-multi)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-i" . consult-imenu)
         ("M-I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package corfu
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally. This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (advice-add #'corfu--post-command :around #'force-debug)
  (global-corfu-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(substring orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t))

(use-package tempel
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
         ("s-<down>" . tempel-next)
         ("s-<up>" . tempel-previous))
  :init
  (setq tempel-path (expand-file-name "~/.config/emacs/templates/*.eld"))
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: we add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;;; end Completion/minibuffer section

(provide 'editor)

;;; editor.el ends here
