;; grab-mac-link
(global-set-key (kbd "C-c g") 'org-mac-grab-link)


;; query-replace
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)


;; magit use other window
(define-key winner-mode-map [remap magit-diff-visit-worktree-file] 'magit-diff-visit-worktree-file-other-window)
(define-key winner-mode-map [remap magit-diff-visit-file] 'magit-diff-visit-file-other-window)


;; json pretty
(add-hook 'json-mode-hook (lambda() (define-key json-mode-map [remap web-beautify-js] 'json-pretty-print-buffer)))


;; load current-buffer
(defun load-buffer-file()
  "load current buffer"
  (interactive)
  (load-file (buffer-file-name)))

(spacemacs/set-leader-keys "fer" 'load-buffer-file)


;; goto project changelog
(defun org-projectile/goto-changelog()
  "goto projectile changelog"
  (interactive)
  (org-projectile-goto-location-for-project (projectile-project-name)))


(spacemacs/set-leader-keys "ps" 'projectile-save-project-buffers)
(define-key evil-normal-state-map (kbd "K") 'manual-entry)


(setq-default evil-escape-key-sequence nil)


(define-key evil-normal-state-map (kbd ",g]") 'helm-gtags-dwim)
(define-key evil-normal-state-map (kbd ",g[") 'helm-gtags-pop-stack)
