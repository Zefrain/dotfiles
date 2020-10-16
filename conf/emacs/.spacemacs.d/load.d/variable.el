;; platnuml
(setq plantuml-jar-path     (concat (shell-command-to-string "echo -ne $(brew --prefix plantuml)") "/libexec/plantuml.jar"))
(setq org-plantuml-jar-path (concat (shell-command-to-string "echo -ne $(brew --prefix plantuml)") "/libexec/plantuml.jar"))


;; winum
(setq winum-scope (quote frame-local))


;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil)


;; set the default encoding system
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;;(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp buffer-file-coding-system)
    (setq buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))


;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))


;; backup-file
(setq backup-directory-alist (quote ((".*" . "~/.spacemacs.d/backup"))))
(setq make-backup-file-name-function (quote backup-file-name-function-custom))


;; ignore web-mode
(configuration-layer/remove-layer 'web-mode)


;; set warning level
(ignore 'defvaralias)
(setq warning-minimum-level ':error)

;; theme
(setq dotspacemacs-mode-line-theme '(all-the-icons :separator 'arrow))


;; line break
(setq truncate-lines t)


;; vc-control
(setq vc-follow-symlinks nil)
(setq vc-follow-symlinks t)


;; org
(setq org-export-with-section-numbers nil)
(setq org-export-with-sub-superscripts nil)
(setq org-export-with-sub-superscripts (quote {}))
(setq org-superstar-headline-bullets-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((latex . t)
   (dot . t)))

;; org-projectile
(with-eval-after-load 'org-projectile
  (setq org-projectile-file "doc/TODOs.org"))

;; org-agenda
(defun push-agenda (file)
  "push file to org-agenda-files"
  (when (file-exists-p file)
    (push file org-agenda-files)))

(with-eval-after-load 'org-agenda
  (require 'org-projectile)
  (mapcar 'push-agenda (org-projectile-todo-files))
  (require 'org-journal)
  (push-agenda org-journal-dir))


;; org-publish
(setq org-publish-project-alist
      '(("orgfiles"
         :base-directory "/Users/zhoush/Documents/Notes/"
         :base-extension "org"
         :publishing-directory "/ssh:root@39.105.199.190:/usr/local/openresty/nginx/html/"
         ;; :publishing-directory "/Users/zhoush/Dropbox/Publish/Notes/"
         :publishing-function org-html-publish-to-html
         :recursive t
         :headline-levels 3
         :section-numbers nil)

        ("images"
         :base-directory "/Users/zhoush/Documents/Notes/"
         :base-extension "svg\\|css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "/ssh:root@39.105.199.190:/usr/local/openresty/nginx/html/"
         ;; :publishing-directory "/Users/zhoush/Dropbox/Publish/Notes/"
         :publishing-function org-publish-attachment
         :recursive t)

        ("blog"
         :components ("orgfiles" "images"))

        ("resume"
         :base-directory "/Users/zhoush/Dropbox/Resume"
         :base-extension "org"
         :publishing-directory "/ssh:root@39.105.199.190:/usr/local/openresty/nginx/html/"
         :publishing-function org-html-publish-to-html
         :section-numbers nil)

        ("journal"
         :base-directory "/Users/zhoush/Documents/Notes/stock/2020"
         :base-extension "org"
         :publishing-directory "/Users/zhoush/Documents/Notes/stock/2020/html"
         :publishing-function org-html-publish-to-html
         :recursive t
         )))


;; projectile
(setq projectile-indexing-method (quote hybrid))
(with-eval-after-load 'projectile
  (delete-dups (setq projectile-globally-ignored-directories
                     (append projectile-globally-ignored-directories '(".ccls-cache" ".mypy_cache" "build" "bin")))))

;; cache
;; (setq spacemacs-cache-directory (concat dotspacemacs-directory "cache/"))
;; (setq spacemacs-auto-save-directory (concat dotspacemacs-directory "auto-save/"))
;; (setq recentf-save-file (concat spacemacs-cache-directory "recentf"))
;; (setq projectile-cache-file (concat spacemacs-cache-directory "projectile.cache"))
;; (setq ccls-initialization-options:cache (concat spacemacs-cache-directory "ccls-cache"))


;; c-c++
(setq c-basic-offset 4)


;; tex
(delete-dups (setq exec-path (append exec-path '("/Library/TeX/texbin"))))


;; python
(setq python-indent-offset 4)
(setq python-shell-interpreter "python3")


;; lua
(with-eval-after-load 'flycheck
  (setq flycheck-luacheck-standards (quote ("ngx_lua"))))


(use-package lsp-lua-emmy
  :demand
  :ensure nil
  :load-path "~/Public/github/lsp-lua-emmy"
  :hook (lua-mode . lsp)
  :config
  (setq lsp-lua-emmy-jar-path (expand-file-name "EmmyLua-LS-all.jar" dotspacemacs-directory))
  )

;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :hook ((lua-mode) . lsp)
;;   :config
;;   )

;; (use-package company-lsp
;;   :ensure t
;;   :after lsp-mode
;;   :config
;;   (setq company-lsp-enable-recompletion t)
;;   (setq lsp-auto-configure nil)         ;该功能会自动执行(push company-lsp company-backends)
;;   )

;; (defun set-company-backends-for-lua()
;;   "Set lua company backend."
;;   (setq-local company-backends '(
;;                                  (
;;                                   company-lua
;;                                   company-lsp
;;                                   company-keywords
;;                                   company-gtags
;;                                   company-yasnippet
;;                                   )
;;                                  company-capf
;;                                  company-dabbrev-code
;;                                  company-files
;;                                  )))

;; (use-package lua-mode
;;   :ensure t
;;   :mode "\\.lua$"
;;   :interpreter "lua"
;;   :hook (lua-mode . set-company-backends-for-lua)
;;   :config
;;   (setq lua-indent-level 4)
;;   (setq lua-indent-string-contents t)
;;   (setq lua-prefix-key nil))


;; company
(custom-set-faces
 '(company-tooltip-common
   ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection
   ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
