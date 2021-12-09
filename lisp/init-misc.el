;;; init-misc.el --- misc tools                      -*- lexical-binding: t; -*-

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

;; misc tools config

;;; Code:

(use-package vterm
  :ensure t
  :bind ("C-c T" . vterm))

(use-package vterm-toggle
  :ensure t
  :after vterm
  :bind ("C-c t" . vterm-toggle))

;; pdftools
(use-package pdf-tools
  :ensure t
  :config
)

(use-package pdf-view-restore
  :ensure t
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))


(use-package treemacs
  :ensure t
  :config
  (treemacs-follow-mode)
  (treemacs-fringe-indicator-mode)
  (treemacs-git-mode 'deferred)
  (treemacs-filewatch-mode))

(provide 'init-misc)
;;; init-misc.el ends here
