;;; init-evil.el --- Config for evil                 -*- lexical-binding: t; -*-

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

;; evil
(use-package evil
  :straight (evil :type git :host github :repo "emacs-evil/evil")
  :init
  (setq-default evil-want-keybinding nil)
  (setq evil-emacs-state-cursor 'bar)
  :bind (:map evil-normal-state-map
              ("K" . helpful-at-point))
  :config
  (defalias 'evil-insert-state 'evil-emacs-state)
  (evil-set-leader 'normal (kbd "SPC"))
  (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)
  (evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-this-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bb") 'counsel-switch-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bi") 'ibuffer)
  (evil-define-key 'normal 'global (kbd "<leader>fD") 'delete-this-file)
  (evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'counsel-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>fr") 'counsel-recentf)
  (evil-define-key 'normal 'global (kbd "<leader>fe") '(lambda () (interactive) (counsel-projectile-find-file "~/.emacs.d/init.el")))
  (evil-define-key 'normal 'global (kbd "<leader>fp") '(lambda () (interactive) (counsel-projectile-find-file "~/.ymacs.d")))
  (evil-define-key 'normal 'global (kbd "<leader>qq") 'ymacs/logout)
  (evil-define-key 'normal 'global (kbd "<leader>gg") 'magit)
  (evil-define-key 'normal 'global (kbd "<leader>:") 'counsel-M-x)
  (evil-define-key 'normal 'global (kbd "<leader>;") 'eval-expression)
  (evil-define-key 'normal 'global (kbd "<leader> SPC") 'counsel-projectile-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>ss") 'swiper-isearch)
  (evil-define-key 'normal 'global (kbd "<leader>ot") 'vterm-toggle)
  (evil-define-key 'normal 'global (kbd "<leader>oe") 'eshell-pop-toggle)
  (evil-define-key 'normal 'global (kbd "<leader>op") 'treemacs)
  (evil-define-key 'normal 'global (kbd "<leader>wh") 'windmove-left)
  (evil-define-key 'normal 'global (kbd "<leader>wl") 'windmove-right)
  (evil-define-key 'normal 'global (kbd "<leader>wk") 'windmove-up)
  (evil-define-key 'normal 'global (kbd "<leader>wj") 'windmove-down)
  (evil-mode))

(use-package evil-collection
  :straight (evil-collection :type git :host github :repo "emacs-evil/evil-collection")
  :config
  (evil-collection-init))

(use-package evil-lispy
  :ensure t
  :config (evil-lispy-mode))

(provide 'init-evil)
;;; init-evil.el ends here
