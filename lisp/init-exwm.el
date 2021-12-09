;;; init-exwm.el -- EXWM config -*-lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package xelb
  :ensure t)

(use-package exwm
  :ensure
  :config
  ;; (define-obsolete-function-alias 'exwm-config-default
  ;;   #'exwm-config-example "27.1")
  (setq exwm-workspace-number 5)
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (setq exwm-input-global-keys
        `(
          ;; 's-r': Reset (to line-mode).
          ([?\s-r] . exwm-reset)
          ;; 's-w': Switch workspace.
          ([?\s-t] . exwm-workspace-switch)
          ;; 's-d': Launch application.
          ([?\s-d] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ([?\s-=] . (lambda ()
                       (interactive)
                       (shell-command "pamixer --allow-boost -i 3")))
          ([?\s--] . (lambda ()
                       (interactive)
                       (shell-command "pamixer --allow-boost -d 3")))

          ([?\s-w] . (lambda ()
                       (interactive)
                       (start-process-shell-command "Brave-browser" nil "brave-bin")))

          ;; 's-N': Switch to certain workspace.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))
  (let ((workspace-numbers (number-sequence 0 9))
        (keys ")!@#$%^&*("))
    (seq-doseq (num workspace-numbers)
      (let* ((idx num)
             (key (aref keys idx)))
        (exwm-input-set-key (kbd (format "s-%c" key))
                            `(lambda ()
                               (interactive)
                               (exwm-workspace-move-window , num)))))))

  (add-hook 'exwm-manage-finish-hook
            (lambda () (call-interactively #'exwm-input-release-keyboard)
              (exwm-layout-hide-mode-line)))
  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])))


(defun yeh/exwm-input-line-mode ()
  "Set exwm window to line-mode and show mode line"
  (call-interactively #'exwm-input-grab-keyboard)
  (exwm-layout-show-mode-line))

(defun yeh/exwm-input-char-mode ()
  "Set exwm window to char-mode and hide mode line"
  (call-interactively #'exwm-input-release-keyboard)
  (exwm-layout-hide-mode-line))

(defun yeh/exwm-input-toggle-mode ()
  "Toggle between line- and char-mode"
  (interactive)
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (if (equal (second (second mode-line-process)) "line")
          (yeh/exwm-input-char-mode)
        (yeh/exwm-input-line-mode)))))

(exwm-input-set-key (kbd "s-i") #'yeh/exwm-input-toggle-mode)

(setq exwm-manage-configurations
      '(((equal exwm-class-name "Brave-browser"
                workspace 1
                char-mode t))
        ((equal exwm-class-name "Google-chrome")
         char-mode t)
        ((equal exwm-class-name "phototshop.exe")
         char-mode t)
        ((equal exwm-class-name "St")
         char-mode t)
        ((equal exwm-class-name "discord")
         workspace 3)
        ((equal exwm-class-name "Gpick")
         floating t
         floating-mode-line nil
         width 0.4
         height 0.5)))

(add-hook 'exwm-floating-setup-hook
          (lambda ()
            (exwm-layout-hide-mode-line)
            (setq floating-mode-line nil)))
;; Make buffer name more meaningful
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name))
          )

(provide 'init-exwm)
;;; init-exwm.el ends here
