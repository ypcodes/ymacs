;;; init-basic.el --- Basic configurations           -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yeh Peng

;; Author: Yeh Peng <yemouren@protonmail.com>

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

;;; Code:

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))

(when (file-exists-p custom-file)
  (load custom-file))

(use-package emacs
  :init
  (setq inhibit-splash-screen 1)
  (setq fancy-startup-text nil)
  (setq make-backup-file nil)
  (setq browse-url-handlers '(("\\`file:" . browse-url-default-browser)))
  (setq-default dired-dwim-target t)
  (setq-default cursor-type 'bar)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq scroll-step            1
        scroll-conservatively  10000)
  (server-start)
  (setq initial-major-mode 'org-mode
        initial-scratch-message "Welcome, happy hacking Ymacs!\n")
  :hook ((prog-mode . linum-mode)
         (prog-mode . electric-pair-mode)
         (prog-mode . prettify-symbols-mode)
         (prog-mode . hl-line-mode)
         (find-file . auto-insert)
         (after-save . delete-trailing-whitespace))
  :config
  (display-time-mode)
  (display-battery-mode)
  (size-indication-mode)
  (column-number-mode)
  (ido-mode)
  (delete-selection-mode 1)
  (set-frame-parameter nil 'alpha 85)
  (add-to-list 'default-frame-alist '(alpha . 85))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq visible-bell nil))))

(use-package recentf
  :config (recentf-mode 1)
  (global-set-key (kbd "C-x C-r") 'recentf-open-files))

(use-package better-defaults
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "http_proxy" "https_proxy"))
    (add-to-list 'exec-path-from-shell-variables var)))

(provide 'init-basic)
;;; init-basic.el ends here
