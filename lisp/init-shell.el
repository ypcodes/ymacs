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
                                 (setq hscroll-margin 0)))

  (define-key vterm-mode-map (kbd "<C-Backspace>") (kbd "C-w"))
  (define-key vterm-mode-map (kbd "<C-c>") #'vterm-send-C-c))

(use-package vterm-toggle
  :ensure t
  :after vterm)

(provide 'init-shell)
;;; init-shell.el ends here
