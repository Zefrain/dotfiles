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


;; reload current config file
(spacemacs/set-leader-keys "fer" 'load-buffer-file)
;; (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "fer" 'load-buffer-file)


(spacemacs/set-leader-keys "ps" 'projectile-save-project-buffers)
(define-key evil-normal-state-map (kbd "K") 'manual-entry)


(setq-default evil-escape-key-sequence nil)

(spacemacs/set-leader-keys-for-minor-mode 'ggtags-mode
  "g]" 'helm-gtags-dwim
  "g[" 'helm-gtags-pop-stack)

;; org-mode
(spacemacs/set-leader-keys-for-major-mode 'org-mode "iT" 'org-insert-todo-return)

