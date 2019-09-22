(add-hook 'c-mode-hook   (lambda () (c-toggle-comment-style -1))) ;;
(add-hook 'sql-mode-hook (lambda () (sql-set-product 'mysql)))
(add-hook 'lua-mode-hook (lambda () (flycheck-luacheck-standards ' ("ngx_lua"))))
(add-hook 'org-mode-hook (lambda () (setq org-confirm-babel-evaluate nil
                                          )))
