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

(use-package lsp-java
  :ensure t
  :config
  (add-hook 'java-mode-hook #'lsp)
  (setq lsp-java-java-path "/usr/bin/java")
  (require 'lsp-java-boot)
  ;; to enable the lenses
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  (setq lsp-java-configuration-runtimes '[
                                          (:name "JavaSE-11"
                                                 :path "/opt/openjdk-bin-17"
                                                 :default t)]))
(use-package dap-java)

(provide 'init-java)
;;; init-java.el ends here
