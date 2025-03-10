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
  (format-buffer-with "gofumpt"))
;; (format-buffer-with "goimports")

(defun go/before-save-hook ()
  "Runs gofumpt on the buffer before saving."
  (setq go-ts-mode-indent-offset 4)
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
  :config
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

;;; go-template --- Major mode for editing Go templates, inheriting from go-ts-mode.

;;; Commentary:

;;; Code:

(require 'go-ts-mode) ;; Ensure go-ts-mode is available

;; Define the syntax highlighting keywords for Go template expressions.
(defvar go-template-keywords
  '(("{{\\(.*?\\)}}" . font-lock-variable-name-face) ;; Go template delimiters
    ("{{\\(if\\|else\\|end\\|range\\|with\\|block\\|define\\|template\\|.\\|len\\|and\\|or\\|not\\)}}"
     . font-lock-keyword-face) ;; Template keywords
    ("\\(true\\|false\\|nil\\)" . font-lock-constant-face) ;; Constants
    ("{{[^{}]+}}" . font-lock-preprocessor-face))) ;; General Go template expression

;; Define the Go template major mode.
(define-derived-mode go-template-mode go-ts-mode "GoTemplate"
  "A major mode for editing Go templates, inheriting from go-ts-mode."
  (font-lock-add-keywords nil go-template-keywords) ;; Add template-specific keywords
  (setq mode-name "GoTemplate")
  (remove-hook 'before-save-hook 'go-format-buffer t)) ;; Disable go-format-buffer when the mode is activated

;; Function to remove go-format-buffer from before-save-hook.
(defun go-template-disable-go-format ()
  (remove-hook 'before-save-hook 'go-format-buffer t)) ;; Buffer-local removal

(add-hook 'go-template-mode-hook 'go-template-disable-go-format)

;; Automatically associate .tmpl files with go-template-mode.
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . go-template-mode))

(provide 'go-template-mode)

;;; go-template ends here

;;; groovy-mode --- groovy-mode configuration.

;;; Commentary:

;;; Code:

(use-package groovy-mode)

;;; groovy-mode ends here

;;; hcl --- hcl configuration.

;;; Commentary:

;;; Code:

(use-package hcl-mode)

;;; hcl ends here

;;; jenkinsfile-mode --- jenkinsfile-mode configuration.

;;; Commentary:

;;; Code:

(use-package jenkinsfile-mode)

;;; jenkinsfile-mode ends here

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

;;; rego --- rego configuration.

;;; Commentary:

;;; Code:

(require 'treesit)

(defconst rego-ts-mode--font-lock-rules
  '(
    :language rego
    :override t
    :feature keyword
    ([(import) (package)] @font-lock-keyword-face
     [(with) (as) (every) (some) (in) (not) (if) (contains) (else) (default)] @font-lock-keyword-face
     "null" @font-lock-keyword-face)

    :language rego
    :feature constant
    (["true" "false"] @font-lock-constant-face
     (number) @font-lock-constant-face)

    :language rego
    :feature operator
    ([(assignment_operator) (bool_operator) (arith_operator) (bin_operator)] @font-lock-operator-face)

    :language rego
    :feature string
    ([(string) (raw_string)] @font-lock-string-face)

    :language rego
    :feature variable
    ((term (ref (var))) @font-lock-variable-name-face
     (expr_call func_arguments: (fn_args (expr) @font-lock-variable-use-face))
     (rule_args (term) @font-lock-variable-use-face))

    :language rego
    :feature comment
    ((comment) @font-lock-comment-face)

    :language rego
    :feature function
    ((expr_call func_name: (fn_name (var) @font-lock-function-name-face))
     (rule (rule_head (var) @font-lock-function-name-face)))

    :language rego
    :feature bracket
    ([(open_paren) (close_paren) (open_bracket) (close_bracket) (open_curly) (close_curly)] @font-lock-delimiter-face)))

(defconst rego-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `rego-ts-mode'.")

(defconst rego-ts-mode--indent-rules
  `((rego
     ;; Basic rules
     ((parent-is "source_file") column-0 0)

     ;; Closing brackets/braces/parentheses should align with their opening counterparts
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)

     ;; After opening brace/bracket/parenthesis, indent the next line
     ((node-is "{") parent-bol ,rego-ts-mode-indent-offset)
     ((node-is "[") parent-bol ,rego-ts-mode-indent-offset)
     ((node-is "(") parent-bol ,rego-ts-mode-indent-offset)

     ;; Special handling for objects, arrays, and sets
     ((parent-is "object") parent-bol ,rego-ts-mode-indent-offset)
     ((parent-is "array") parent-bol ,rego-ts-mode-indent-offset)
     ((parent-is "set") parent-bol ,rego-ts-mode-indent-offset)

     ;; Rules for other constructs
     ((parent-is "rule_body") parent-bol ,rego-ts-mode-indent-offset)
     ((parent-is "query") parent-bol ,rego-ts-mode-indent-offset)
     ((parent-is "literal") parent-bol ,rego-ts-mode-indent-offset)
     ((parent-is "expr") parent-bol 0)

     ;; Comments align with the previous line
     ((node-is "comment") prev-line 0)

     ;; Default case
     (no-node parent-bol 0)))
  "Tree-sitter indent rules for `rego-ts-mode'.")

(define-derived-mode rego-ts-mode prog-mode "rego[ts]"
  "Major mode for editing Rego code, using tree-sitter."

  ;; Ensure tree-sitter is available and the grammar is loaded
  (when (treesit-ready-p 'rego)
    (treesit-parser-create 'rego)

    ;; Configure font-lock
    (setq-local treesit-font-lock-settings
                (apply #'treesit-font-lock-rules
                       rego-ts-mode--font-lock-rules))

    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (keyword string constant)
                  (function variable module)
                  (operator bracket)))

    ;; Configure indentation
    (setq-local treesit-simple-indent-rules rego-ts-mode--indent-rules)

    ;; Enable tree-sitter
    (treesit-major-mode-setup)))

;; Register file association
(add-to-list 'auto-mode-alist '("\\.rego\\'" . rego-ts-mode))

(provide 'rego-ts-mode)

;;; rego ends here

;;; sql-indent --- sql-indent configuration.

;;; Commentary:

;;; Code:

(use-package sql-indent)

;;; sql-indent ends here

;;; swift --- swift configuration.

;;; Commentary:

;;; Code:

(use-package swift-mode)

;;; swift ends here

;;; yaml --- yaml configuration.

;;; Commentary:

;;; Code:

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

;;; zig --- zig configuration.

;;; Commentary:

;;; Code:

(use-package zig-mode)

;;; zig ends here

(provide 'lang)
;;; lang.el ends here
