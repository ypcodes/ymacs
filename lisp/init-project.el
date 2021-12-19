;;; init-project.el --- Project management           -*- lexical-binding: t; -*-

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

(use-package ag
  :ensure t)

(use-package projectile
  :ensure t
  :hook ((prog-mode . projectile-mode)))

(use-package counsel-projectile
  :ensure t)

(use-package magit
  :ensure t)

(use-package treemacs
  :ensure t
  :config
  (treemacs-follow-mode)
  (treemacs-fringe-indicator-mode)
  (treemacs-git-mode 'deferred)
  (treemacs-filewatch-mode))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

(use-package treemacs-all-the-icons
  :ensure t
  :after (treemacs)
  :config
  (treemacs-load-all-the-icons-with-workaround-font "Hermit"))

(use-package treemacs-icons-dired
  :ensure t
  :after dired
  :hook (dired-mode . treemacs-icons-dired-mode))

(use-package treemacs-projectile
  :ensure t
  :config
  )

(provide 'init-project)
;;; init-project.el ends here
