(define-key evil-motion-state-map (kbd "C-c c ,") #'my-lsp/find-references)
(define-key evil-motion-state-map (kbd "C-c c .") #'my-lsp/find-definitions)
