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

;; (use-package org-plus-contrib)

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
        org-tags-column 5
        org-directory "~/org/")
  :hook ((org-mode . org-indent-mode)
         (org-mode . display-fill-column-indicator-mode)
         (org-mode . hl-line-mode)
         (org-mode . yas-minor-mode)
         )
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
     (ocaml . nil)
     (octave . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (shell . t)
     (sql . t)
     (mathematica . t)
     (sqlite . t)))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 3))
  (add-to-list 'org-src-lang-modes '("mathematica" . wolfram))
)

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

(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-leading-bullet ?\s
        org-superstar-leading-fallback ?\s
        org-hide-leading-stars nil
        org-superstar-todo-bullet-alist '(("TODO" . 9744)
                                          ("[ ]"  . 9744)
                                          ("DONE" . 9745)
                                          ("[X]"  . 9745))))

(use-package org-pdftools
  :ensure t
  :after org
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚑" "⬆" "■")))

(use-package org-books
  :ensure t
  :after org
  :config
  (setq org-books-file "~/org/booklists.org"))

(use-package literate-calc-mode
  :ensure t
  :after (org)
  :hook (org-mode . literate-calc-minor-mode))

(use-package ox-pandoc
  :ensure t
  :after org
  :init
  ;; default options for all output formats
  (setq org-pandoc-options '((standalone . t)))
  ;; cancel above settings only for 'docx' format
  (setq org-pandoc-options-for-docx '((standalone . nil)))
  ;; special settings for beamer-pdf and latex-pdf exporters
  (setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
  (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))
  ;; special extensions for markdown_github output
  (setq org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html)))

(provide 'init-org)
;;; init-org.el ends here
