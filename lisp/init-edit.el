;;; init-edit.el --- Edit config                     -*- lexical-binding: t; -*-

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

(use-package multiple-cursors
  :ensure t
  :bind ("M-n" . mc/mark-next-like-this-symbol)
  :bind ("M-p" . mc/mark-previous-like-this-symbol)
  :bind ("C-M-n" . mc/mark-next-like-this)
  :bind ("C-M-p" . mc/mark-previous-like-this))

;; goto-line
(use-package goto-line-preview
  :ensure t
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))

(provide 'init-edit)
;;; init-edit.el ends here
