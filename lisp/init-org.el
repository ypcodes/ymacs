;;; init-org.el --- org config                       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yeh Peng

;; Author: Yeh Peng <yemouren@protonmail.com>
;; Keywords: extensions

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

;; org config

;;; Code:

(use-package org
  :init
  ;; Various preferences
  (setq org-log-done t
        org-edit-timestamp-down-means-later t
        org-hide-emphasis-markers t
        org-catch-invisible-edits 'show
        org-export-coding-system 'utf-8
        org-fast-tag-selection-single-key 'expert
        org-html-validation-link nil
        org-export-kill-product-buffer-when-displayed t
        org-tags-column 80)
  :config
  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (with-eval-after-load 'org-agenda
    (add-hook 'org-agenda-mode-hook
              (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))
  (org-babel-do-load-languages
   'org-babel-load-languages
    '((R . t)
      (C . t)
      (ditaa . t)
      (dot . t)
      (emacs-lisp . t)
      (eshell . t)
      (gnuplot . t)
      (haskell . nil)
      (latex . t)
      (ledger . t)
      (ocaml . nil)
      (octave . t)
      (plantuml . t)
      (python . t)
      (ruby . t)
      (screen . nil)
      (shell . t)
      (sql . t)
      (sqlite . t))))

(use-package org-tempo
  :after org)

(use-package org-cliplink
  :ensure t
  :after org)

(use-package toc-org
  :ensure t
  :after org
  :hook
  (org-mode . toc-org-mode))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  )

(use-package org-pdftools
  :ensure t
  :after org
  :hook (org-mode . org-pdftools-setup-link)
  )

(provide 'init-org)
;;; init-org.el ends here
