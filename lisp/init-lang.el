;;; init-lang.el --- Chinese configuration           -*- lexical-binding: t; -*-

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

;; chinese configuration

;;; Code:

(use-package rime
  :ensure t
  :init
  (setq default-input-method "rime")
  :config
  (setq rime-user-data-dir "~/.config/fcitx/rime")
  (setq rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdcccx"
              :internal-border-width 10))
  (setq rime-show-candidate 'minibuffer)
  (setq rime-inline-ascii-trigger 'shift-l)
  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
  (setq mode-line-mule-info '((:eval (rime-lighter))))
  (setq rime-disable-predicates
        '(rime-predicate-evil-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-prog-in-code-p)))

;; check your grammar
(use-package langtool
  :ensure t
  :init
  (setq langtool-bin "/usr/bin/languagetool")
  (setq langtool-mother-tongue "en")
  :config
  (defun langtool-autoshow-detail-popup (overlays)
  (when (require 'popup nil t)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))
  (setq langtool-autoshow-message-function
        'langtool-autoshow-detail-popup))

(provide 'init-lang)
;;; init-lang.el ends here
