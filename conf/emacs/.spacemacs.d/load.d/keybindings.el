;; grab-mac-link
(add-hook 'org-mode-hook (lambda()
                           (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

(define-key winner-mode-map [remap magit-diff-visit-file-worktree] 'magit-diff-visit-worktree-file-other-window)
