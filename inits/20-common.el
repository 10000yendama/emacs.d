;; common.el

(keyboard-translate ?\C-h ?\C-?)
(add-to-list 'load-path (locate-user-emacs-file "elisp"))

;; show line numbers if Emacs version >= 26
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode))

;; indentation settings for C source code
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)
(add-hook 'c-mode-common-hook '(lambda ()
                                 (c-set-style "k&r")
                                 ;; the default value for k&r is 5
                                 (setq c-basic-offset 4) 
                                 (show-paren-mode t)
                                 (setq show-trailing-whitespace t)))

(require 'epa-file)
(epa-file-enable)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key "D09D9078")

;; sensitive-mode
(require 'sensitive-mode)
(add-to-list 'auto-mode-alist '("\\.org\\'" . sensitive-mode))
(defun org-mode-hooks ()
  (sensitive-mode)
  (auto-fill-mode))
(add-hook 'org-mode-hook 'org-mode-hooks)

;; common packages
(use-package amx
  :config
  (amx-mode))
(use-package ccls
  :custom (ccls-executable "~/local/bin/ccls")
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))
(use-package company
  :init 
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-selection-wrap-around t)
  (global-company-mode))
(use-package company-lsp :commands company-lsp)
(use-package counsel
  :diminish counsel-mode
  :config (counsel-mode 1))
(use-package ddskk-autoloads
  :straight ddskk
  :bind (("C-x C-j" . skk-mode)
         ("C-x j" . skk-auto-fill-mode)
         ("C-x t" . skk-turotial))
  :init
  :config
  (setq skk-init-file (locate-user-emacs-file ".skk")
        default-input-method "japanese-skk"))
(use-package diminish)
(use-package flymake)
(use-package htmlize)
(use-package ivy
  :diminish ivy-mode
  :config (ivy-mode 1))
(use-package lsp-mode
  :commands lsp
  :init)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package swiper
  :bind (("C-s" . swiper)))
(use-package tex
  :straight auctex)
(use-package visual-regexp-steroids
  :bind (("M-%" . vr/query-replace)
         ("C-M-r" . vr/isearch-backward)
         ("C-M-s" . vr/isearch-forward))
  :config
  (setq vr/engine 'python))
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))
