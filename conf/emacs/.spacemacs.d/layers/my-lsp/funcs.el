(defun my-lsp/find-definitions ()
  (interactive)
  (if lsp-mode (lsp-ui-peek-find-definitions) (spacemacs/jump-to-definition)))

(defun my-lsp/find-references ()
  (interactive)
  (if lsp-mode (lsp-ui-peek-find-references) (spacemacs/jump-to-reference)))

(defun my-lsp/jump-backward ()
  (interactive)
  (if lsp-mode (lsp-ui-peek-jump-backward)))

(defun my-lsp/jump-forward ()
  (interactive)
  (if lsp-mode (lsp-ui-peek-jump-forward)))

(defun my-lsp/imenu ()
  (interactive)
  (if lsp-mode (lsp-ui-imenu) (imenu-list)))
