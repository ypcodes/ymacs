;;; init-cc.el --- c config                           -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yeh Peng

;; Author: Yeh Peng <yemouren@protonmail.com>
;; Keywords: c

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

;; C config

;;; Code:

;; CCLS

(use-package ccls
  :after (cc-mode)
  :ensure t
  :config
  (setq ccls-executable "/usr/bin/ccls")
  (ccls-use-default-rainbow-sem-highlight))

(use-package clang-format
  :ensure t
  :after cc)

(provide 'init-cc)
;;; init-c.el ends here
