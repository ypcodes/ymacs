;;; init-exwm.el -- EXWM config -*-lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defvar exwm--toggle-workspace 0
  "Previously selected workspace. Used with `exwm/jump-to-last-exwm'.")

(defun exwm/jump-to-last-exwm ()
  "Jump to last window."
  (interactive)
  (exwm-workspace-switch exwm--toggle-workspace))

(defadvice exwm-workspace-switch
    (before save-toggle-workspace activate)
  (setq exwm--toggle-workspace exwm-workspace-current-index))

(defun yeh/exwm-input-line-mode ()
  "Set exwm window to line-mode and show mode line."
  (call-interactively #'exwm-input-grab-keyboard)
  (exwm-layout-show-mode-line))

(defun yeh/exwm-input-char-mode ()
  "Set exwm window to char-mode and hide mode line."
  (call-interactively #'exwm-input-release-keyboard)
  (exwm-layout-hide-mode-line))

(defun yeh/exwm-input-toggle-mode ()
  "Toggle between line- and char-mode."
  (interactive)
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (if (equal (second (second mode-line-process)) "line")
          (yeh/exwm-input-char-mode)
        (yeh/exwm-input-line-mode)))))

(use-package xelb
  :ensure t)

(use-package exwm
  :ensure t
  :init
  (setq exwm-workspace-number 5)
  (setq mouse-autoselect-window t
        focus-follows-mouse t)
  (setq window-divider-default-right-width 1)
  :config
  (require 'exwm-config)
  (progn
    (exwm-input-set-key (kbd "<s-tab>")  #'exwm/jump-to-last-exwm)
    (exwm-input-set-key (kbd "s-w")  #'(lambda ()
                                         (interactive)
                                         (start-process-shell-command
                                          "Brave-browser" nil "brave-bin")))
    (exwm-input-set-key (kbd "s-d") #'(lambda (command)
                                        (interactive (list (read-shell-command "Command: ")))
                                        (start-process-shell-command command nil command)))
    (exwm-input-set-key (kbd "s-=") #'desktop-environment-volume-increment)
    (exwm-input-set-key (kbd "s--") #'desktop-environment-volume-decrement)
    (exwm-input-set-key (kbd "s-i") #'yeh/exwm-input-toggle-mode)
    (mapcar (lambda (i)
              (exwm-input-set-key (kbd (format "s-%d" i))
                                  #'(lambda ()
                                      (interactive)
                                      (exwm-workspace-switch-create i))))
            (number-sequence 0 9))
    )

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (add-hook 'exwm-manage-finish-hook
            (lambda () (call-interactively #'exwm-input-release-keyboard)
              (exwm-layout-hide-mode-line)))

  (add-hook 'exwm-floating-setup-hook
            (lambda ()
              (setq floating-mode-line nil)))
  ;; Make buffer name more meaningful
  (add-hook 'exwm-update-class-hook
            (lambda () (exwm-workspace-rename-buffer exwm-class-name)))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-title)))
  (exwm-enable))

(use-package exwm-systemtray
  :after exwm
  :config
  (exwm-systemtray-enable))

(use-package desktop-environment
  :ensure t
  :after exwm
  :init
  (setq desktop-environment-screenshot-directory "~/Pictures/screenshot"
        desktop-environment-update-exwm-global-keys :global)
  :config
  (desktop-environment-mode))

(use-package xdg
  :ensure t
  :commands (xdg-config-dirs xdg-config-home xdg-desktop-read-file))

(provide 'init-exwm)
;;; init-exwm.el ends here
