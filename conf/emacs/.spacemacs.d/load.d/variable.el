;; platnuml
(setq plantuml-jar-path     (concat (shell-command-to-string "echo -ne $(brew --prefix plantuml)") "/libexec/plantuml.jar"))
(setq org-plantuml-jar-path (concat (shell-command-to-string "echo -ne $(brew --prefix plantuml)") "/libexec/plantuml.jar"))


;; winum
(setq winum-scope (quote frame-local))


;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil)



;; set the default encoding system
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

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
(setq org-export-with-sub-superscripts nil)
(setq org-export-with-sub-superscripts (quote {}))
(setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))
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
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "/ssh:root@39.105.199.190:/usr/local/openresty/nginx/html/"
         ;; :publishing-directory "/Users/zhoush/Dropbox/Publish/Notes/"
         :publishing-function org-publish-attachment
         :recursive t)

        ("blog" :components ("orgfiles" "images"))

        ("resume"
         :base-directory "/Users/zhoush/Dropbox/Resume"
         :base-extension "org"
         :publishing-directory "/ssh:root@39.105.199.190:/usr/local/openresty/nginx/html/"
         :publishing-function org-html-publish-to-html
         :section-numbers nil)))


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


;; c-style
(setq c-basic-offset 4)


;; tex
(delete-dups (setq exec-path (append exec-path '("/Library/TeX/texbin"))))


;; python
(setq python-indent-offset 4)
