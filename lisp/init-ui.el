;;; init-ui.el -- ui config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'use-package)

(use-package doom-themes
  :ensure t
  :if (display-graphic-p)
  :config
  (if emacs-theme
      (load-theme emacs-theme t)
    (load-theme 'doom-one t)))

;; make your theme like emacs
(use-package theme-magic
  :after (doom-themes)
  :if (display-graphic-p)
  :ensure t
  :config
  (theme-magic-export-theme-mode))

;; modeline
(use-package minions
  :ensure t
  :config
  (minions-mode))

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

;; tabbar
(use-package centaur-tabs
  :ensure t
  :if (display-graphic-p)
  :init (setq centaur-tabs-set-icons t)
  :config
  (setq centaur-tabs-style "wave")
  (centaur-tabs-mode t))

(use-package counsel
  :ensure t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper-isearch)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-symbol
  :ensure t
  :hook (prog-mode . highlight-symbol-mode))

(use-package winum
  :ensure t
  :config
  (winum-mode))

(use-package dtrt-indent
  :ensure t
  :hook (prog-mode .
                   (lambda ()
                     (modify-syntax-entry ?_ "w")
                     (dtrt-indent-mode)
                     (dtrt-indent-adapt))))

(provide 'init-ui)

;;; init-ui.el ends here
