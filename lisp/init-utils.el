;;; init-utils.el -- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defadvice find-file (around find-files activate)
  "Also find all files within a list of files. This even works recursively."
  (if (listp filename)
      (loop for f in filename do (find-file f wildcards))
    ad-do-it))

;; Delete current editing file
(defun delete-this-file ()
  "Delete current editing file and close buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is editting"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; Browse current HTML file
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;; Rename the current file

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun ymacs/shutdown ()
  "Shutdown now."
  (interactive)
  (shell-command "sudo shutdown -h now"))

(defun ymacs/restart ()
  "Reboot now."
  (interactive)
  (shell-command "sudo reboot"))

(defun ymacs/logout ()
  "Logout from exwm."
  (interactive)
  (bookmark-save)
  (recentf-save-list)
  (save-some-buffers)
  (save-buffers-kill-terminal))

(defun ymacs/open-emacs-init-file ()
  "Open ~/.emacs.d/init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun ymacs/open-user-config-file ()
  "Open ~/.emacs.d/init.el."
  (interactive)
  (find-file "~/.ymacs.d/config.el"))

(provide 'init-utils)
;;; init-utils.el ends here
