;; grab-mac-link
(add-hook 'org-mode-hook (lambda()
                           (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

;; c-c++
(add-hook 'c-mode-hook (lambda()
                         (define-key c-mode-map (kbd "C-c =")
                           (lambda ()
                             (interactive)
                             (spacemacs/clang-format-region-or-buffer)
                             (if (region-active-p)
                                 (align (region-beginning) (region-end)))))))
