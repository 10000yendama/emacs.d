;; common.el

(keyboard-translate ?\C-h ?\C-?)
(add-to-list 'load-path (locate-user-emacs-file "elisp"))

;; show line numbers if Emacs version >= 26
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode))

;; hide tool-bar
(tool-bar-mode -1)

;; remove minor-mode indicator for specific modes
(use-package diminish)

(use-package asm-mode
  :ensure nil
  :hook (asm-mode . (lambda () (setq indent-tabs-mode nil
				     tab-width 4))))

;; indentation settings for C source code
(use-package cc-mode
  :ensure nil
  :config (setq c-default-style "k&r"
		c-basic-offset 4
                tab-width 4
                indent-tabs-mode nil
		show-trailing-whitespace t)
  :hook (c-mode-common . (lambda ()
                           (show-paren-mode t))))

;; org-related packages
(use-package epa
  :ensure nil
  :config (epa-file-enable))

(require 'sensitive-mode)

(use-package org
  :ensure nil
  :hook (org-mode . (lambda ()
		      (auto-fill-mode)
		      (sensitive-mode)))
  :config
  (require 'org-crypt)
  (setq org-tags-exclude-from-inheritance (quote ("crypt"))
	org-crypt-key "D09D9078")
  (org-crypt-use-before-save-magic))

;; help learn key combinations
(use-package amx
  :config
  (amx-mode))

;; LSP (language server protocol) related packages
(use-package lsp-mode
  :commands lsp
  :init)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package ccls
  :custom (ccls-executable "~/local/bin/ccls")
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

;; completion
(use-package company
  :init 
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-selection-wrap-around t)
  (global-company-mode))
(use-package company-lsp :commands company-lsp)

;; better M-x, C-x b, C-x C-f, ...
(use-package ivy
  :diminish ivy-mode
  :config (ivy-mode 1))
(use-package counsel
  :diminish counsel-mode
  :config (counsel-mode 1))
(use-package swiper
  :bind (("C-s" . swiper)))

;; input Japanese
(use-package ddskk-autoloads
  :straight ddskk
  :bind (("C-x C-j" . skk-mode)
         ("C-x j" . skk-auto-fill-mode)
         ("C-x t" . skk-turotial))
  :init
  :config
  (setq skk-init-file (locate-user-emacs-file ".skk")
        default-input-method "japanese-skk"))

(use-package flymake)
(use-package htmlize)
(use-package tex
  :straight auctex)
(use-package visual-regexp
  :demand t
  :commands (vr/query-replace vr/isearch-backward vr/isearch-forward)
  :bind (("M-%" . vr/query-replace)
         ("C-M-r" . vr/isearch-backward)
         ("C-M-s" . vr/isearch-forward))
  :config
  (use-package visual-regexp-steroids
    :demand)
  (setq vr/engine 'python))
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))
