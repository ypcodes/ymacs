;;; init-ui.el -- ui config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package doom-themes
  :ensure t
  :config
  (if emacs-theme
      (load-theme emacs-theme t)
    (load-theme 'doom-one t)))

;; make your theme like emacs
(use-package theme-magic
  :after (doom-themes)
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
  :config
  (setq centaur-tabs-style "wave")
  (centaur-tabs-mode t))

(use-package smex
  :ensure t
  :config
  (setq smex-prompt-string "Smex: ")
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(use-package ido-grid-mode
  :after smex
  :ensure t
  :init
  (setq ido-grid-mode-max-columns 7
        ido-grid-mode-max-rows 1
        ido-grid-mode-prefix-scrolls t
        ido-grid-mode-scroll-down #'ido-grid-mode-next-row
        ido-grid-mode-scroll-up #'ido-grid-mode-previous-row
        ido-grid-mode-order nil
        ido-grid-mode-start-collapsed t)
  :config
  (ido-grid-mode))

;; highlight
(use-package symbol-overlay
  :ensure t
  :hook (prog-mode . symbol-overlay-mode))

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

(provide 'init-ui)

;;; init-ui.el ends here
