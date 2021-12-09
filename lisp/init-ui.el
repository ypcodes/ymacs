;;; init-ui.el -- ui config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package doom-themes
  :ensure t
  :config
  (if emacs-theme
      (load-theme emacs-theme t)
    (load-theme 'doom-one t)))

;; battery
(use-package fancy-battery
  :ensure t
  :config
  (add-hook 'after-init-hook #'fancy-battery-mode)
)

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
  (setq smex-prompt-string "M-x: ")
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package ido
  :config (ido-mode))

(use-package ido-grid-mode
  :after smex
  :ensure t
  :init
  (setq ido-grid-mode-max-columns 7
        ido-grid-mode-max-rows 0
        ido-grid-mode-prefix-scrolls t
        ido-grid-mode-scroll-down #'ido-grid-mode-next-row
        ido-grid-mode-scroll-up #'ido-grid-mode-previous-row
        ido-grid-mode-order nil
        ido-grid-mode-start-collapsed t)
  :config
  (ido-grid-mode))

(provide 'init-ui)
;;; init-ui.el ends here
