(setq cquery-executable "/usr/local/bin/cquery")

(add-hook 'c-mode-hook #'lsp-cquery-enable)
(add-hook 'c++-mode-hook #'lsp-cquery-enable)
(add-hook 'python-mode-hook #'lsp-python-enable)
