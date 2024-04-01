;;; go --- go configuration.

;;; Commentary:

;;; Functions:

(defun gofmt ()
  "Format the current buffer with gofumpt and goimports."
  (interactive)
  (format-buffer-with "gofumpt")
  (format-buffer-with "goimports"))

(defun go/before-save-hook ()
  "Runs gofumpt on the buffer before saving."
  (add-hook 'before-save-hook #'gofmt -10 t))

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

;;; Hooks:

;;; Code:

(use-package go-ts-mode
  :straight nil
  :hook (go-ts-mode . go/before-save-hook)
  :bind (:map go-ts-mode-map
              ("C-c l f" . #'gofmt))
  :init
  (setq go-ts-mode-indent-offset 4))

(use-package go-mode
  :hook (go-mode . go/before-save-hook)
  :bind (:map go-ts-mode-map
              ("C-c l f" . #'gofmt)))

(use-package go-tag
  :config
  (setq go-tag-args (list "-transform" "camelcase"))
  (with-eval-after-load 'go-ts-mode
    (define-key go-ts-mode-map (kbd "C-c t") #'go-tag-add)
    (define-key go-ts-mode-map (kbd "C-c T") #'go-tag-remove)))

(provide 'go)

;;; go.el ends here
