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

;;; Hooks:

(add-hook 'before-save-hook 'xah-clean-whitespace)

;;; Code:

;; Disable bars.
(scroll-bar-mode    nil)
(tool-bar-mode      nil)
(menu-bar-mode      nil)
(column-number-mode)

;; Set default tab width.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Set fill column to 80 by default (for comments only).
(setq-default fill-column 80)

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

(use-package crux
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
	     ([(shift return)] . crux-smart-open-line)
	     ("C-<backspace>" . crux-kill-line-backwards)
	     ("C-c n" . crux-cleanup-buffer-or-region)
	     ("C-c e" . crux-eval-and-replace)
	     ("C-c D" . crux-delete-file-and-buffer)
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
         ("C-S-k" . puni-backward-kill-line))
  :config
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  (puni-global-mode)
  (electric-pair-mode))

;; (use-package smartparens
;;   :diminish smartparens-mode
;;   :commands (smartparens-mode smartparens-strict-mode)
;;   :bind ("<backspace>" . backward-delete-char)
;;   :init
;;   (require 'smartparens-config)
;;   (setq sp-show-pair-from-inside t
;;         sp-show-pair-delay 0
;;         sp-message-width nil)
;;   (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
;;   (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
;;   (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))
;;   (smartparens-global-strict-mode))

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

(use-package avy
  :bind (("C-c j" . avy-goto-line)
         ("s-j" . avy-goto-char-timer)))

(use-package flymake
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (flymake-mode))

(use-package eglot
  :bind (("C-c l e" . eglot)
         (:map eglot-mode-map
               ("C-c l r" . eglot-rename)
               ("C-c l a" . eglot-code-actions)
               ("C-c l f" . eglot-format-buffer)))
  :init
  (setq-default eglot-workspace-configuration
                '((:gopls .
                          ((staticcheck . t)
                           (matcher . "Fuzzy")
                           (gofumpt . t)
                           (analyses . ((fieldalignment . t)
                                        (nilness . t)
                                        (shadow . t)
                                        (unusedwrite . t)
                                        (useany . t)
                                        (unusedvariable . t)))))))
  :config
  (add-to-list 'eglot-server-programs '((c-ts-mode c++-ts-mode) "clangd"))
  (add-to-list 'eglot-server-programs '((typescript-ts-mode tsx-ts-mode) "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs '(bash-ts-mode "bash-language-server" "start"))
  (add-to-list 'eglot-server-programs '(css-ts-mode "vscode-css-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs '(dockerfile-ts-mode "docker-langserver" "--stdio"))
  (add-to-list 'eglot-server-programs '(go-ts-mode "gopls"))
  (add-to-list 'eglot-server-programs '(html-ts-mode "vscode-html-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs '(json-ts-mode "vscode-json-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs '(python-ts-mode ""))
  (add-to-list 'eglot-server-programs '(python-ts-mode "pyright-langserver" "--stdio"))
  (add-to-list 'eglot-server-programs '(rust-ts-mode "rust-analyzer"))
  (add-to-list 'eglot-server-programs '(yaml-ts-mode "yaml-language-server" "--stdio")))

(use-package marginalia
  :bind ((:map minibuffer-local-map
               ("M-A" . marginalia-cycle)))
  :custom
  (marginalia-max-relative-ago 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive))
  :hook ((minibuffer-setup . vertico-repeat-save)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :bind ((:map vertico-map
               ("<tab>" . vertico-insert)
               ("<escape>" . minibuffer-keyboard-quit)
               ("<backspace>" . vertico-directory-delete-char)
               ("C-w" . vertico-directory-delete-word)
               ("C-<backspace>" . vertico-directory-delete-word)
               ("<return>" . vertico-directory-enter)))
  :custom
  (veritco-count 13)
  (vertico-resize t)
  (vertico-cycle nil)
  :config
  (vertico-mode)
  (vertico-indexed-mode)
  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand))))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-s" . consult-line)
         ("C-x p b" . consult-project-buffer)
         ("M-g f" . consult-flymake)
         ("M-g i" . consult-imenu)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :init
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package consult-eglot)

(use-package affe
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))

(use-package corfu
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-popupinfo))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :bind ((:map corfu-map
               ([tab] . corfu-next)
               ("C-n" . corfu-next)
               ("C-p" . corfu-previous)
               ("<escape>" . corfu-quit)
               ("<return>" . corfu-insert)
               ("M-d" . corfu-show-documentation)
               ("M-l" . corfu-show-location)
               ("M-n" . corfu-doc-scroll-up)
               ("M-p" . corfu-doc-scroll-down)))
  :custom
  ;; Smart tab behavior - indent or complete.
  (tab-always-indent 'complete)

  ;; Always show all candidates in popup menu.
  (completion-cycle-threshold nil)

  ;; Only use corfu when calling `completion-at-point' or `indent-for-tab-command'.
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 14)
  (corfu-cycle t)
  ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
  ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
  ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
  ;; `corfu-separator' and a keybind for `corfu-insert-separator', which my
  ;; configuration already has pre-prepared). Necessary for manual corfu usage with
  ;; orderless, otherwise first component is ignored, unless `corfu-separator'
  ;; is inserted.
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s) ;; use space
  (corfu-quit-no-match 'separator) ;; don't quit if there is `corfu-separator' inserted
  (corfu-preview-current 'insert) ;; preview first candidate
  (corfu-preselect 'prompt)
  (corfu-preselect-first t)
  (corfu-echo-documentation nil)

  ;; corfu-popupinfo
  (corfu-doc-delay 0)
  (corfu-doc-max-width 70)
  (corfu-doc-max-height 20)
  :init
  (global-corfu-mode)
  :config
  ;; Enable Corfu more generally for every minibuffer, as long as no other
  ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
  ;; completion UI. From
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

(use-package kind-icon
  :if (display-graphic-p)
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ;; to compute blended backgrounds correctly
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  ;; Add hook to reset cache so the icon colors match my theme.
  (add-hook 'my/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package cape
  :bind (([tab] . completion-at-point)
         ("C-c p t" . complete-tag)
         ("C-c p d" . cape-dabbrev)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  (add-to-list 'completion-at-point-functions #'eglot-completion-at-point)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-line)
  (add-to-list 'completion-at-point-functions #'cape-symbol))

(provide 'editor)

;;; editor.el ends here
