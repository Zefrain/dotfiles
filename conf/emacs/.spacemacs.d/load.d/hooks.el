(add-hook 'c-mode-hook   (lambda ()
                           ;; (c-toggle-comment-style -1) ;;
                           (setq comment-style (quote indent))
                           (setq flycheck-clang-language-standard "c11")))

(add-hook 'c++-mode-hook (lambda ()
                           (setq flycheck-clang-language-standard "c++11")))

(add-hook 'sql-mode-hook (lambda () (sql-set-product 'mysql)))

;; (add-hook 'lua-mode-hook (lambda () (flycheck-luacheck-standards ' ("ngx_lua"))))

(add-hook 'org-mode-hook (lambda () (setq org-confirm-babel-evaluate nil)))
