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
(require 'init-shell)

(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
(define-key global-map (kbd "C-c c f") #'ymacs/open-emacs-init-file)
(define-key global-map (kbd "C-c c c") #'ymacs/open-user-config-file)
(define-key global-map (kbd "<s-return>") #'eshell)
(define-key global-map (kbd "C-c o t") #'vterm-toggle)
(define-key global-map (kbd "C-c o p") #'treemacs)
(define-key global-map (kbd "C-c p f") #'projectile-find-file)
(define-key global-map [remap save-buffers-kill-terminal] #'ymacs/exwm-logout)
(global-set-key (kbd "<C-return>") (kbd "C-e C-m"))
(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c k")  'windmove-down)

;; (global-set-key [remap isearch-forward-regexp] #'swiper-isearch)
;; escape cancels all
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; evil
(use-package evil
  :straight (evil :type git :host github :repo "emacs-evil/evil")
  :init
  (setq-default evil-want-keybinding nil)
  (setq evil-emacs-state-cursor 'bar)
  :bind (:map evil-normal-state-map
              ("/" . swiper-isearch)
              ("K" . helpful-at-point))
  :config
  (defalias 'evil-insert-state 'evil-emacs-state)
  (evil-set-leader 'normal (kbd "SPC"))
  (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)
  (evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-this-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bb") 'counsel-switch-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'counsel-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>fr") 'recentf-open-more-files)
  (evil-define-key 'normal 'global (kbd "<leader>qq") 'ymacs/exwm-logout)
  (evil-define-key 'normal 'global (kbd "<leader>:") 'counsel-M-x)
  (evil-define-key 'normal 'global (kbd "<leader> SPC") 'counsel-projectile-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>ot") 'vterm-toggle)
  (evil-define-key 'normal 'global (kbd "<leader>oe") 'eshell-pop-toggle)
  (evil-define-key 'normal 'global (kbd "<leader>op") 'treemacs)
  (evil-mode))

(use-package evil-collection
  :straight (evil-collection :type git :host github :repo "emacs-evil/evil-collection")
  :config
  (evil-collection-init))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
