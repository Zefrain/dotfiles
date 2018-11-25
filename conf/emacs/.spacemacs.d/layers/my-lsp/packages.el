;;; packages.el --- my-lsp layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: zhoushang <zhoush@zhoushangs-MacBook-Pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-lsp-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-lsp/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-lsp/pre-init-PACKAGE' and/or
;;   `my-lsp/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-lsp-packages
          '(cquery company-lsp lsp-go (lsp-python :require lsp-mode)))
;;; packages.el ends here
(defun my-lsp/post-init-cquery()
  (use-package cquery
    :defer t
    :init
    (setq cquery-executable "/usr/local/bin/cquery"))
  (add-hook 'c-mode-hook #'lsp-cquery-enable)
  (add-hook 'c++-mode-hook #'lsp-cquery-enable))

(defun my-lsp/post-init-lsp-go()
  (use-package lsp-go)
  (add-hook 'go-mode-hook #'lsp-go-enable))

(defun my-lsp/post-init-lsp-python()
  (add-hook 'python-mode-hook #'lsp-python-enable))

(defun my-lsp/post-init-company-lsp()
  (use-package company-lsp
    :defer t
    :init
    (setq company-quickhelp-delay 0)
    ;; Language servers have better idea filtering and sorting,
    ;; don't filter results on the client side.
    (setq company-transformers nil
          company-lsp-async t
          company-lsp-cache-candidates nil)
    (spacemacs|add-company-backends :backends company-lsp :modes c-mode c++-mode python-mode go-mode)
    ) ;; lsp
  )
