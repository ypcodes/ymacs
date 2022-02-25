;;; init-math.el --- Mathematics config              -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Peng Yeh

;; Author: Peng Yeh <yemouren@protonmail.com>
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

(use-package wolfram
  :ensure t
  :config
  (autoload 'wolfram-mode "wolfram-mode" nil t)
  (autoload 'run-wolfram "wolfram-mode" nil t)
  (setq wolfram-program "/opt/Wolfram/Mathematica/13.0/Executables/MathKernel")
  (add-to-list 'auto-mode-alist '("\.m$" . wolfram-mode))
  (setq wolfram-path "~/.Mathematica/Applications"))

(provide 'init-math)
;;; init-math.el ends here
