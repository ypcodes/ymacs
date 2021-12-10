;;; init-java.el --- Java config                     -*- lexical-binding: t; -*-

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

;; java config

;;; Code:

;; lsp-java maybe a better choice, but java-11 on gentoo is unavailable.
(use-package meghanada
  :ensure t
  :config
  (add-hook 'java-mode-hook
            (lambda ()
              ;; meghanada-mode on
              (meghanada-mode t)
              (flycheck-mode +1)
              (setq c-basic-offset 2)
              ;; use code format
              (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
  (setq meghanada-java-path "java")
  (setq meghanada-maven-path "mvn"))

(provide 'init-java)
;;; init-java.el ends here
