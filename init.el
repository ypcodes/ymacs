;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")
                         ("melpa" . "http://elpa.zilongshanren.com/melpa/")))
(package-initialize)

;; add ~/.emacs.d/lisp to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name ".ymacs.d" (getenv "HOME")))

(defvar emacs-theme nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(defconst *cc-mode-use-lsp* t)

(eval-when-compile
  (require 'site-gentoo)
  (require 'init-pacmanage)
  (require 'config)
  (require 'init-ui)
  (require 'init-utils)
  (require 'init-isearch)
  (require 'init-company)
  (require 'init-c)
  (require 'init-java)
  (require 'init-org)
  (require 'init-misc)
  (require 'init-exwm)
  )

(use-package emacs
  :init
  (setq inhibit-splash-screen 1)
  (setq fancy-startup-text nil)
  (setq make-backup-file nil)
  (setq user-full-name "Yeh Peng"
        user-mail-address "yemouren@protonmail.com")
  (setq browse-url-handlers '(("\\`file:" . browse-url-default-browser)))
  (setq-default dired-dwim-target t)
  (setq-default cursor-type 'bar)
  :config
  (ido-mode)
  (delete-selection-mode 1)
  (set-frame-parameter nil 'alpha 85)
  (add-to-list 'default-frame-alist '(alpha . 85))

  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq visible-bell nil)))
  (add-hook 'after-save-hook 'delete-trailing-whitespace)
  (add-hook 'find-file-hook 'auto-insert))

(use-package recentf
  :config
  (recentf-mode 1)
  (global-set-key (kbd "C-x C-r") 'recentf-open-files))

;; helpful
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command))
  )

(use-package better-defaults
  :ensure t)

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-startup-hook 'enable-paredit-mode)
  )

(use-package find-file-in-project
  :ensure t
  :bind ("C-c C-f" . find-file-in-project)
  :config
  (setq ffip-prefer-ido-mode t))

(use-package magit
  :ensure t)

;; lispy mode
(use-package lispy
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode))

;;; folding
(use-package origami
  :ensure t
  :init (global-origami-mode t)
  :config
  (with-eval-after-load 'origami
    (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
    (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes))
  )

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

(exwm-enable)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
