;;; init-shell.el --- SHell configuration            -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yeh Peng

;; Author: Yeh Peng <yemouren@protonmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'esh-mode)

(defun eshell/mkcd (dir)
  "Create DIR then cd into it."
  (make-directory dir t)
  (eshell/cd dir))

(defun eshell/gst (&rest args)
  (magit-status (pop args) nil)
  (eshell/echo))

(defun +eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (eshell/clear-scrollback)
  (eshell-emit-prompt))

(defun ha/eshell-quit-or-delete-char (arg)
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp))
      (progn
        (eshell-life-is-too-much)       ; Why not? (eshell/exit)
        (ignore-errors
          (delete-window)))
    (delete-forward-char arg)))

(use-package vterm
  :ensure t
  :when (bound-and-true-p module-file-suffix)
  :commands vterm-mode
  :init
  (when noninteractive
    (advice-add #'vterm-module-compile :override #'ignore)
    (provide 'vterm-module))
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 6000)
  (add-hook 'vterm-mode-hook #'(lambda ()
                                 (setq confirm-kill-processes nil)
                                 (setq hscroll-margin 0))))

(use-package vterm-toggle
  :ensure t
  :after vterm)

(use-package eshell
  :init
  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t)
  (add-hook 'eshell-mode-hook
            '(lambda ()
              (add-to-list 'eshell-visual-commands "ssh")
              (add-to-list 'eshell-visual-commands "tail")
              (add-to-list 'eshell-visual-commands "top")))
  (add-hook 'eshell-mode-hook
            '(lambda ()
               (bind-keys :map eshell-mode-map
                          ("C-d" . ha/eshell-quit-or-delete-char)
                          ("C-l" . +eshell/clear))))

  (add-hook 'eshell-mode-hook (lambda ()
                                (eshell/alias "ff" "find-file $1")
                                (eshell/alias "emacs" "find-file $1")
                                (eshell/alias "ee" "find-file-other-window $1")

                                (eshell/alias "gd" "magit-diff-unstaged")
                                (eshell/alias "gds" "magit-diff-staged")
                                (eshell/alias "d" "dired $1")
                                (eshell/alias "c" "eshell/clear-scrollback")

                                ;; The 'ls' executable requires the Gnu version on the Mac
                                (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                                              "/usr/local/bin/gls"
                                            "/bin/ls")))
                                  (eshell/alias "ll" (concat ls " -AlohG --color=always"))))))

(use-package eshell-git-prompt
  :ensure t
  :after eshell
  :init (eshell-git-prompt-use-theme 'powerline))

(use-package eshell-syntax-highlighting
  :after eshell
  :ensure t ;; Install if not already installed.
  :config
  ;; Enable in all Eshell buffers.
  (add-hook 'eshell-mode-hook 'eshell-syntax-highlighting-mode))

(use-package eshell-did-you-mean
  :ensure t
  :after eshell
  :config
  (eshell-did-you-mean-setup)
  ;; HACK There is a known issue with `eshell-did-you-mean' where it does not
  ;;      work on first invocation, so we invoke it once manually by setting the
  ;;      last command and then calling the output filter.
  (setq eshell-last-command-name "catt")
  (eshell-did-you-mean-output-filter "catt: command not found"))

;; quickly cd to parent dir
(use-package eshell-up
  :ensure t
  :init (setq eshell-up-print-parent-dir t)
  :after eshell
  :config
  (eshell/alias "up" "eshell-up $1")
  (eshell/alias "pk" "eshell-up-peek $1"))

;; popup eshell
(use-package pop-eshell-mode
  :straight (pop-eshell-mode :type git :host github :repo "stanhe/pop-eshell" :files ("*.el"))
  :after eshell
  :config
  (setq pop-find-parent-directory '(".git" "gradlew")) ;; parent directory should have .git or gradlew file
  (pop-eshell-mode 1)
  (define-key global-map (kbd "C-c o e") 'eshell-pop-toggle))

(provide 'init-shell)
;;; init-shell.el ends here
