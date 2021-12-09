;;; init-company.el --- complete config              -*- lexical-binding: t; -*-

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

;; complete config

;;; Code:

;; company mode
(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

;; template
(use-package yasnippet
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  )

(use-package doom-snippets
  :load-path "~/.emacs.d/elpa/doom-snippets"
  :after yasnippet)

;; lsp
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (cc-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq ccls-sem-highlight-method 'overlay)
  ;; (ccls-call-hierarchy t)
  )

;; optionally
(use-package lsp-ui :ensure t :commands lsp-ui-mode)

(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :ensure t
  :config
  (require 'dap-lldb)
  (setq dap-lldb-debug-program '("/usr/bin/lldb-vscode"))
  (setq dap-lldb-debugged-program-function (lambda () (read-file-name "Select file to debug.")))
  (defun dap-debug-create-or-edit-json-template ()
    "Edit the C++ debugging configuration or create + edit if none exists yet."
    (interactive)
    (let ((filename (concat (lsp-workspace-root) "/launch.json"))
	  (default "~/.emacs.d/default-launch.json"))
      (unless (file-exists-p filename)
	(copy-file default filename))
      (find-file-existing filename)))
  )
(use-package dap-lldb)

(provide 'init-company)
;;; init-company.el ends here
