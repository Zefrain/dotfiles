(setq plantuml-jar-path     (concat (shell-command-to-string "echo -ne $(brew --prefix plantuml)") "/libexec/plantuml.jar"))
(setq org-plantuml-jar-path (concat (shell-command-to-string "echo -ne $(brew --prefix plantuml)") "/libexec/plantuml.jar"))

;; winum
(setq winum-scope (quote frame-local))

(setq org-export-with-sub-superscripts nil)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; backup-file
(setq backup-directory-alist (quote ((".*" . "~/.spacemacs.d/backup"))))
;; (setq make-backup-file-name-function (quote backup-file-name-function-custom))
(setq org-export-with-sub-superscripts (quote {}))

;; (prefer-coding-system 'gb2312-dos)
;; (setq default-buffer-file-coding-system 'gb2312-dos)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(configuration-layer/remove-layer 'web-mode)

(setq comment-style 'indent)
(ignore 'defvaralias)


(setq warning-minimum-level ':error)
(setq dotspacemacs-mode-line-theme '(all-the-icons :separator 'arrow))


;; line break
(setq truncate-lines t)

(setq vc-follow-symlinks nil)
