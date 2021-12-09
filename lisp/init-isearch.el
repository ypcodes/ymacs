;;; init-isearch.el -- Isearch settings -*- lexical-binding: t -*t
;;; Commentary:
;;; Code:


(use-package anzu
  :ensure t
  :config
  (anzu-mode t)
  (add-hook 'after-init-hook 'global-anzu-mode)
  (setq anzu-mode-lighter "")
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace))

;; isearch
(use-package isearch
  :config
  (with-eval-after-load 'isearch
    ;; DEL during isearch should edit the search string, not jump back to the previous result
    (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

    ;; Activate occur easily inside isearch
    (when (fboundp 'isearch-occur)
      ;; to match ivy conventions
      (define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur))
    )

  (defun yeh/isearch-exit-other-end ()
    "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
    (interactive)
    (isearch-exit)
    (goto-char isearch-other-end))

  (define-key isearch-mode-map [(control return)] 'yeh/isearch-exit-other-end)
  )


(provide 'init-isearch)
;;; init-isearch.el ends here
