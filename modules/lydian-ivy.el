;;; lydian-ivy.el -- ivy is a powerful alternative to the popular ido-mode.

;;; Commentary:

;;; Code:

(use-package ivy
    :init
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (setq projectile-completion-system 'ivy)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    :config
    (ivy-mode 1))

;; swiper provides enhanced buffer search
(use-package swiper
    :init
    (global-set-key "\C-s" 'swiper))

;; counsel supercharges a lot of commands with some ivy magic
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

(provide 'lydian-ivy)

;;; lydian-ivy.el ends here
