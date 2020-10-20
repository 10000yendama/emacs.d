;; common.el

(define-key key-translation-map [?\C-h] [?\C-?])
(add-to-list 'load-path (locate-user-emacs-file "elisp"))

;; show line numbers if Emacs version >= 26
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode))

;; hide tool-bar
(tool-bar-mode -1)

;; don't use tabs
(setq-default indent-tabs-mode nil)

;; apply theme and customize modeline color
(when (window-system)
  ;; theme
  (when (window-system)
    (use-package color-theme-sanityinc-tomorrow
      :config
      (load-theme 'sanityinc-tomorrow-eighties t)))
  (set-face-foreground 'mode-line "dim gray")
  (set-face-background 'mode-line "goldenrod1")
  (set-face-foreground 'mode-line-buffer-id "blue")
  (set-face-foreground 'mode-line-inactive "#999999")
  (set-face-background 'mode-line-inactive "#595959"))

;; remove minor-mode indicator for specific modes
(use-package diminish)
(defmacro safe-diminish (file mode &optional new-name)
  "https://github.com/larstvei/dot-emacs/blob/master/init.org"
  `(with-eval-after-load ,file
     (diminish ,mode ,new-name)))

;; use safe-diminish only for non-use-package minor modes.
(safe-diminish "eldoc" 'eldoc-mode)

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
  :bind (("S-<iso-lefttab>" . org-shifttab))
  :config
  (require 'org-crypt)
  (setq org-tags-exclude-from-inheritance (quote ("crypt"))
	org-crypt-key "D09D9078")
  (org-crypt-use-before-save-magic))

;; help learn key combinations
(use-package amx
  :config
  (amx-mode))

;; to find project root
(use-package projectile)

;; LSP (language server protocol) related packages
;; Install python-language-server[all] via pipx to make it work.
;; Preferably inject the following packages
;;      - black
;;      - pyls-black
;;      - pyls-isort (should be <5? cannot make it work :( )
;;      - pyls-mypy
(use-package lsp-mode
  :commands lsp
  :init
  :config
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  (require 'lsp-pyls)
  (setq lsp-pyls-plugins-pylint-enabled t)
  (add-hook 'python-mode-hook 'lsp))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package ccls
  :custom (ccls-executable "~/local/bin/ccls")
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

;; completion
(use-package company
  :init
  :diminish company-mode
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-selection-wrap-around t)
  (global-company-mode))
(use-package company-lsp :commands company-lsp)

;; to prevent viperize ask every time Emacs launches
(setq viper-mode nil)

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
  :bind (("C-x C-j" . skk-auto-fill-mode)
         ("C-x j" . skk-auto-fill-mode)
         ("C-x t" . skk-turotial))
  :init
  :config
  (setq skk-init-file (locate-user-emacs-file ".skk")
        default-input-method "japanese-skk")
  ;; dired-x uses C-x C-j by default
  (add-hook 'dired-load-hook
            (lambda ()
              (load "dired-x")
              (global-set-key (kbd "C-x C-j") 'skk-auto-fill-mode))))

(use-package magit)
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

;; py-autopep8
;; (use-package py-autopep8
;;   :hook (python-mode . py-autopep8-enable-on-save))

(defun timestamps-for-markdown ()
  (interactive)
  (org-element-map (org-element-parse-buffer) 'clock
    (lambda (x)
      (let* ((ts (org-element-property :value x))
             (drawer (org-element-property :parent x))
             (sec (org-element-property :parent drawer))
             (hl (org-element-property :parent sec))
             (title (org-element-property :raw-value hl))
             (colon (lambda (s) (if s (concat (substring s 0 2) ":" (substring s 2 4)) "-")))
             (pstart) (pend) (astart) (aend) (out))
        (if (string-match "\\([0-9]\\{4\\}\\)?-\\([0-9]\\{4\\}\\)? \\(.+\\)" title)
            (setq pstart (funcall colon (match-string 1 title))
                  pend (funcall colon (match-string 2 title))
                  title (match-string 3 title))
          (setq pstart "-" pend "-" body title))
        (setq astart (funcall colon (format "%02d%02d" (org-element-property :hour-start ts)
                                            (org-element-property :minute-start ts)))
              aend (funcall colon (format "%02d%02d" (org-element-property :hour-end ts)
                                          (org-element-property :minute-end ts))))
        (setq out (format "| %s | %s | %s | %s | %s |\n" pstart pend title astart aend))
        (insert out)))))

