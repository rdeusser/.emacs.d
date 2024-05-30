;;; -*- lexical-binding: t -*-
;;; lang.el --- all language configuration lives here.

;; Author: Robert Deusser

;;; Commentary:

;;; Code:

;;; bash --- bash configuration.

;;; Commentary:

;;; Code:

(add-hook 'bash-ts-hook-mode 'eglot-ensure)
(add-hook 'sh-hook-mode 'eglot-ensure)

;;; bash ends here

;;; c --- c configuration.

;;; Commentary:

;;; Code:

(setq c-mode-basic-offset 4)

;;; c ends here

;;; dockerfile --- dockerfile configuration.

;;; Commentary:

;;; Code:

(use-package dockerfile-ts-mode
  :straight nil
  :init
  (defun dockerfile/disable-aggressive-indent ()
    "Disables aggressive indent mode because it's annoying in Dockerfiles."
    (aggressive-indent 0))

  (add-hook 'dockerfile-ts-mode 'dockerfile/disable-aggressive-indent))

;;; dockerfile ends here

;;; editorconfig --- editorconfig configuration.

;;; Commentary:

;;; Code:

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;;; editorconfig ends here

;;; fish --- fish configuration.

;;; Commentary:

;;; Code:

(use-package fish-mode)

;;; fish ends here

;;; go --- go configuration.

;;; Commentary:

;;; Code:

(defun go-format-buffer ()
  "Format the current buffer with gofumpt and goimports."
  (interactive)
  (format-buffer-with "gofumpt")
  (format-buffer-with "goimports"))

(defun go/before-save-hook ()
  "Runs gofumpt on the buffer before saving."
  (add-hook 'before-save-hook #'go-format-buffer -10 t))

(defun go-add-import (arg import)
  "Add a new IMPORT to the list of imports.

When called with a prefix ARG asks for an alternative name to
import the package as.

If no list exists yet, one will be created if possible.

If an identical import has been commented, it will be
uncommented, otherwise a new import will be added."
  ;; - If there's a matching `// import "foo"`, uncomment it
  ;; - If we're in an import() block and there's a matching `"foo"`, uncomment it
  ;; - Otherwise add a new import, with the appropriate syntax
  (interactive
   (list
    current-prefix-arg
    (replace-regexp-in-string "^[\"']\\|[\"']$" "" (completing-read "Package: " (go-packages)))))
  (save-excursion
    (let (as line import-start)
      (if arg
          (setq as (read-from-minibuffer "Import as: ")))
      (if as
          (setq line (format "%s \"%s\"" as import))
        (setq line (format "\"%s\"" import)))
      (goto-char (point-min))
      (if (re-search-forward (concat "^[[:space:]]*//[[:space:]]*import " line "$") nil t)
          (uncomment-region (line-beginning-position) (line-end-position))
        (cl-case (go-goto-imports)
          (fail (message "Could not find a place to add import."))
          (block-empty
           (insert "\n\t" line "\n"))
          (block
           (save-excursion
             (re-search-backward "^import (")
             (setq import-start (point)))
           (if (re-search-backward (concat "^[[:space:]]*//[[:space:]]*" line "$")  import-start t)
               (uncomment-region (line-beginning-position) (line-end-position))
             (insert "\n\t" line)))
          (single (insert "import " line "\n"))
          (none (insert "\nimport (\n\t" line "\n)\n")))))))

(defun gopp ()
  "Adds github.com/k0kubun/pp/v3 to the list of imports for debugging."
  (interactive)
  (go-add-import nil "github.com/k0kubun/pp/v3"))

(use-package go-ts-mode
  :straight nil
  :hook (go-ts-mode . go/before-save-hook)
  :bind (:map go-ts-mode-map
              ("C-c l f" . #'go-format-buffer))
  :init
  (setq go-ts-mode-indent-offset 4))

(use-package go-mode
  :hook (go-mode . go/before-save-hook)
  :bind (:map go-mode-map
              ("C-c l f" . #'go-format-buffer)))

(use-package go-tag
  :config
  (setq go-tag-args (list "-transform" "camelcase"))
  (with-eval-after-load 'go-ts-mode
    (define-key go-ts-mode-map (kbd "C-c t") #'go-tag-add)
    (define-key go-ts-mode-map (kbd "C-c T") #'go-tag-remove)))

;;; go ends here

;;; hcl --- hcl configuration.

;;; Commentary:

;;; Code:

(use-package hcl-mode)

;;; hcl ends here

;;; json --- json configuration.

;;; Commentary:

;;; Code:

(defun jqfmt (start end)
  "Format the current selecton or buffer with jq."
  (interactive "r")
  (if (use-region-p)
      (call-process-region start end "jq" t t t)
    (call-process-region (point-min) (point-max) "jq" t t t)))

(defun jqfmt-minify (start end)
  "Format the current selection or buffer with jq and minify the result."
  (interactive "r")
  (if (use-region-p)
      (call-process-region start end "jq" t t t)
    (call-process-region (point-min) (point-max) "jq" t t t "-c")))

;;; json ends here

;;; jsonnet --- jsonnet configuration.

;;; Commentary:

;;; Code:

(use-package jsonnet-mode)

(add-to-list 'auto-mode-alist '("\\.jsonnet\\'" . jsonnet-mode))
(add-to-list 'auto-mode-alist '("\\.libsonnet\\'" . jsonnet-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(jsonnet-mode . ("jsonnet-language-server"))))

;;; jsonnet ends here

;;; liquid --- liquid configuration.

;;; Commentary:

;;; Code:

(setq liquidKeywords
      '(
        ;;; core stuff
	    ("{%\\|%}\\|{{\\|}}" . font-lock-comment-face) ;;; liquid tag delimiters
	    ("{%\s*\\(assign\\|capture\\|endcapture\\|for\\|endfor\\|if\\|endif\\|comment\\|endcomment\\|else\\|elsif\\|unless\\|endunless\\|case\\|when\\|endcase\\|cycle\\)" (1 font-lock-keyword-face)) ;;; liquid construct tags
	    ("forloop" . font-lock-keyword-face)
	    ("forloop.\\(length\\|index0\\|index\\|rindex0\\|rindex\\|first\\|last\\)" (1 font-lock-variable-name-face))
	    ("{%\s*\\(?:assign\\|capture\\|for\\|if\\|unless\\|case\\|when\\)\s+\\(\\(?:\\w\\|\\.\\|_\\)+\\)" (1 font-lock-variable-name-face)) ;;; variable after assign|capture|for|if

	    ("{{\s*\\(\\(?:\\w\\|\\.\\)+\\)" (1 font-lock-variable-name-face)) ;;; variable/object being outputted

	    ;;; filter stuff (hack, only supports 2 chained filters)
	    ("\s+|\s+" . font-lock-comment-face) ;;; liquid tag delimiters
	    ("{{\s*\\(?:\\w\\|\\.\\)+\s+|\s+\\(\\w+\\)" (1 font-lock-type-face)) ;;; variable after assign|capture|for|if
	    ("{{\s*\\(?:\\w\\|\\.\\)+\s+|\s+\\w+\s+|\s+\\(\\w+\\)" (1 font-lock-type-face)) ;;; variable after assign|capture|for|if

	    ;;; if/else stuff
	    ("{%\s*\\(?:if\\|unless\\)\s+\\(?:\\w\\|\\.\\)+\s+\\(contains\\|>\\|<\\|==\\|!=\\)" (1 font-lock-keyword-face)) ;;; liquid operators

	    ;;; for loop stuff
	    ("{%\s*for\s+\\w+\s+\\(in\\)" (1 font-lock-keyword-face)) ;;; the 'in' in "for temp in collection"
	    ("{%\s*for\s+\\w+\s+in\s+\\(\\(?:\\w\\|\\.\\|_\\)+\\)" (1 font-lock-variable-name-face)) ;;; the 'collection' in "for temp in collection"
	    )
      )

(define-derived-mode liquid-mode html-mode
  (setq font-lock-defaults '(liquidKeywords))
  (setq mode-name "liquid mode"))

;;; liquid ends here

;;; protobuf --- protobuf configuration.

;;; Commentary:

;;; Code:

(use-package protobuf-mode)

;;; protobuf ends here

;;; python --- python configuration.

;;; Commentary:

;;; Code:

(use-package python-ts-mode
  :straight nil
  :hook (python-base-mode . python/disable-clean-whitespace)
  :init
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode))
  (defun python/disable-clean-whitespace ()
    "Disables the xah-clean-whitepsace before-save-hook because
it will nest functions under classes when I don't want them to be."
    (remove-hook 'before-save-hook 'xah-clean-whitespace)))

;;; python ends here

;;; yaml --- yaml configuration.

;;; Commentary:

;;; Code:

(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(defun yqfmt (start end)
  "Format the current selecton or buffer with yq."
  (interactive "r")
  (setq current-point (point))
  (if (use-region-p)
      (call-process-region start end "yq" t t t)
    (call-process-region (point-min) (point-max) "yq" t t t))
  (goto-char current-point))

(defun yqfmt-pretty (start end)
  "Format the current selecton or buffer with yq -P."
  (interactive "r")
  (setq current-point (point))
  (if (use-region-p)
      (call-process-region start end "yq" t t t)
    (call-process-region (point-min) (point-max) "yq" t t t "-P"))
  (goto-char current-point))

;;; yaml ends here

(provide 'lang)
;;; lang.el ends here
