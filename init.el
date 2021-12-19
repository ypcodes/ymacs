;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)

;; add ~/.emacs.d/lisp to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name ".ymacs.d" (getenv "HOME")))

(defvar emacs-theme nil "What Emacs theme do you use.")
(defalias 'yes-or-no-p 'y-or-n-p)
(defconst *use-exwm* t)
(defconst *use-evil* t)

(eval-when-compile
  (require 'site-gentoo)
  (require 'init-pacmanage)
  (require 'init-benchmark)
  (require 'init-basic)
  (if *use-exwm*
      (require 'init-exwm))
  (require 'config nil t)
  (require 'init-edit)
  (require 'init-undo)
  (require 'init-fold)
  (require 'init-lang)
  (require 'init-ui)
  (require 'init-utils)
  (require 'init-shell)
  (require 'init-project)
  (require 'init-company)
  (require 'init-cc)
  (require 'init-java)
  (require 'init-elisp)
  (require 'init-org)
  (require 'init-misc)
  (require 'init-media)
  (require 'init-keybindings)
  (if *use-evil*
      (require 'init-evil)))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

;;; init.el ends here
