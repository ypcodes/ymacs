;;; init-keybindings.el --- Define keybindings       -*- lexical-binding: t; -*-

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

;; define keys

;;; Code:

(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)

(define-key global-map (kbd "C-c c f") #'(lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(define-key global-map (kbd "C-c c c") #'(lambda () (interactive) (find-file "~/.ymacs.d/config.el")))
(define-key global-map (kbd "<s-return>") #'vterm)
(define-key global-map (kbd "C-c o t") #'vterm-toggle)
(define-key global-map (kbd "C-c o p") #'treemacs)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
