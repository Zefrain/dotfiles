(add-hook 'c-mode-hook (lambda() (setq-local comment-style 'aligned)))
(add-hook 'c-mode-hook   (lambda () (c-toggle-comment-style -1))) ;;
(add-hook 'sql-mode-hook (lambda () (sql-product 'mysql)))
(add-hook 'lua-mode-hook (lambda () (flycheck-luacheck-standards ' ("ngx_lua"))))
(add-hook 'org-mode-hook (lambda ()
                           (setq org-confirm-babel-evaluate nil)))
