;; grab-mac-link
(define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)

;; cnfonts font size
(global-set-key [remap spacemacs/scale-up-font] 'cnfonts-increase-fontsize)
(global-set-key [remap spacemacs/scale-down-font] 'cnfonts-decrease-fontsize)

;; query-replace
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

;; magit use other window
(define-key winner-mode-map [remap magit-diff-visit-file-worktree] 'magit-diff-visit-worktree-file-other-window)

;; load current-buffer
(defun load-buffer-file()
  "load current buffer"
  (interactive)
  (load-file (buffer-file-name)))
(spacemacs/set-leader-keys "fer" 'load-buffer-file)
