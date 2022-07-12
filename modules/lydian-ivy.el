;;; lydian-ivy.el -- ivy is a powerful alternative to the popular ido-mode.

;;; Commentary:

;;; Code:

(use-package ivy
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  ;; Set ivy height relative to frame height.
  (ivy-height-alist
   '((t
      lambda (_caller)
      (/ (frame-height) 2))))
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq projectile-completion-system 'ivy)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  :config
  (ivy-mode t))

(use-package ivy-rich
  :after ivy)

(use-package ivy-hydra
  :after ivy)

(use-package ivy-avy
  :after ivy)

(use-package ivy-prescient
  :after ivy)

(use-package flx)

;; Swiper provides enhanced buffer search.
(use-package swiper
  :init
  (global-set-key "\C-s" 'swiper))

;; Counsel supercharges a lot of commands with some ivy magic.
(use-package counsel
  :init
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode t))

(provide 'lydian-ivy)

;;; lydian-ivy.el ends here
