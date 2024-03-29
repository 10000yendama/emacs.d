;; common.el  -*- lexical-binding: t; -*-

(define-key key-translation-map [?\C-h] [?\C-?])
(add-to-list 'load-path (locate-user-emacs-file "elisp"))

;; show line numbers if Emacs version >= 26
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode +1))

(if (version<= "28.1" emacs-version)
    (global-display-fill-column-indicator-mode))

;; hide tool-bar
(tool-bar-mode -1)

;; don't use tabs
(setq-default indent-tabs-mode nil)

;; don't create lock files
(setq create-lockfiles nil)

;; nice
(setq require-final-newline t)

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

;; highlight whitespaces
(setq whitespace-style '(face trailing indentation::space tab-mark missing-newline-at-eof))
(global-whitespace-mode +1)

;; setup path correctly
(use-package exec-path-from-shell)

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

;; yasnippet
(use-package yasnippet
  :config (yas-global-mode +1))
(use-package yasnippet-snippets)

;; Flymake
(use-package flymake
  :config
  ;; (require 'cspell-flymake)
  ;; (setq flymake-cspell-executable (executable-find "cspell"))
  :hook
  (prog-mode . (lambda ()
                 (unless (or (eq major-mode 'typescript-mode)
                             (eq major-mode 'web-mode))
                   ;; (cspell-setup-flymake-backend)
                   (flymake-mode)))))

;; LSP (language server protocol) related packages

;; Install pyright and make sure it is in PATH.
;;   pipx install pyright
;; Alternatively you can use pylsp. Install them using:
;;   pipx install python-lsp-server[all]
;;   pipx inject python-lsp-server pylsp-mypy pyls-isort python-lsp-black

(use-package eglot
  :config
  (require 'pylint-flymake)
  :hook (eglot-managed-mode
         . (lambda ()
             (when (and (eglot-managed-p)
                        (eq major-mode 'python-mode)
                        (projectile-current-project-buffer-p)
                        (projectile-project-root)
                        (eq (projectile-project-type) 'python-poetry))
               ;; Automatically setup required dir-local variables:
               ;;   - flymake-pylint-executable :: required to enable pylint
               ;;       backend. If missing pylint backend will fail and
               ;;       become disabled.
               ;;   - blacken-executable :: required to enable black (blacken).
               ;;       If missing blacken-mode will not enabled.
               ;;   - python-isort-command :: required to enable isort (python-isort).
               ;;       If missing python-isort-on-save-mode will not enabled.
               ;;   - elgot-workspace-configuration :: required to use
               ;;       python executable in venv, instead of system-wide one.
               ;;       Without setting up this correctly, import statements
               ;;       will emit "Imports ... could not be resolved" errors.
               ;;
               ;; Automatic setup is only supported for poetry
               ;; projects. For other types of Python projects, you can
               ;; still set up these variables manually.
               ;; (use M-x projectile-edit-dir-locals)
               (let ((symbol (intern (projectile-project-root))))
                 ;; Setup (this will only executed once for a project)
                 (unless (assq symbol dir-locals-class-alist)
                   (let* ((path
                           (string-trim (shell-command-to-string "poetry env info -p")))
                          (pylint-path (format "%s/bin/pylint" path))
                          (black-path (format "%s/bin/black" path))
                          (isort-path (format "%s/bin/isort" path))
                          (dir-local-vars '()))
                     (message (format "Automatically detected venv: %s" path))
                     (when (file-executable-p pylint-path)
                       (message "pylint found; added to dir-local vars ✨")
                       (setf (alist-get 'flymake-pylint-executable dir-local-vars)
                             pylint-path))
                     (when (file-executable-p black-path)
                       (message "black found; added to dir-local vars ✨")
                       (setf (alist-get 'blacken-executable dir-local-vars)
                             black-path))
                     (when (file-executable-p isort-path)
                       (message "isort found; added to dir-local vars ✨")
                       (setf (alist-get 'python-isort-command dir-local-vars)
                             isort-path))
                     (setf (alist-get 'eglot-workspace-configuration dir-local-vars)
                           `(:python (:analysis
                                      (:typeCheckingMode "strict")
                                      :pythonPath
                                      ,(format "%s/bin/python" path))))
                     (dir-locals-set-class-variables symbol
                                                     `((python-mode . ,dir-local-vars))))
                   (dir-locals-set-directory-class (projectile-project-root) symbol)

                   ;; required to reload variables defined just now
                   (hack-dir-local-variables-non-file-buffer))
                 ;; Setup ends here

                 ;; If black executable is found in the setup, enable it right now.
                 (when (assq 'blacken-executable
                             (assq 'python-mode
                                   (assq symbol dir-locals-class-alist)))
                   (blacken-mode 1))

                 ;; If isort executable is found in the setup, enable it right now.
                 (when (assq 'python-isort-command
                             (assq 'python-mode
                                   (assq symbol dir-locals-class-alist)))
                   (python-isort-on-save-mode 1))

                 ;; eglot removes all existing backends, so cspell backend
                 ;; must be added again
                 ;; (cspell-setup-flymake-backend)

                 ;; This will add pylint flymake backend.
                 (pylint-setup-flymake-backend))))))

(use-package blacken)
(use-package python-isort
  :init
  (advice-add 'risky-local-variable-p
              :around
              (lambda (oldfun &rest r)
                (cond ((eq (car r) 'python-isort-command) nil)
                      ((apply oldfun r))))))

;; completion
(use-package company
  :init
  :diminish company-mode
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-selection-wrap-around t)
  (global-company-mode +1))

;; to prevent viperize ask every time Emacs launches
(setq viper-mode nil)

;; better M-x, C-x b, C-x C-f, ...
(use-package ivy
  :diminish ivy-mode
  :config (ivy-mode 1))
(use-package counsel
  :diminish counsel-mode
  :config
  (counsel-mode 1)
  (setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) ""))
(use-package swiper
  :bind (("C-s" . swiper)))

(use-package projectile
  :config
  (projectile-update-project-type
   'python-poetry
   :precedence 'high))

(use-package counsel-projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (counsel-projectile-mode +1))

;; input Japanese
(use-package ddskk-autoloads
  :straight ddskk
  :bind (("C-x C-j" . skk-mode)
         ("C-x j" . skk-mode)
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

(use-package magit
  :bind (("C-c g" . magit-status))
  :config
  (setq magit-process-password-prompt-regexps
        (cons (rx bol "パスフレーズを入力: " eol)
              magit-process-password-prompt-regexps)))

(use-package flymake)
(use-package htmlize)
(use-package tex
  :straight auctex
  :config (setq TeX-engine 'luatex))
(use-package visual-regexp
  :demand t
  :commands (vr/query-replace vr/isearch-backward vr/isearch-forward)
  :bind (("M-%" . vr/query-replace)
         ("C-M-r" . vr/isearch-backward)
         ("C-M-s" . vr/isearch-forward))
  :config
  (use-package visual-regexp-steroids
    :demand)
  (setq vr/engine 'python)
  (setq
   vr/command-python
   (replace-regexp-in-string "^python " "python3 " vr/command-python)))
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package csv-mode)

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

;; YAML
(use-package yaml-mode)

;; tide (TypeScript)
(defun setup-tide-mode ()
  (interactive)
  ;; reload dir-locals (required to set tide-tsserver-executable
  ;; locally, which is required in yarn PnP environment)
  (hack-dir-local-variables-non-file-buffer)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package typescript-mode
  :ensure t
  :hook (typescript-mode . setup-tide-mode))

(use-package tide
  :ensure t
  :config
  (setq company-tooltip-align-annotations t
        typescript-indent-level 2)
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . setup-tide-mode)
         (before-save . tide-format-before-save)))

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  ;; :config
  ;; enable typescript-tslint checker
  ;; (flycheck-add-mode 'typescript-tslint 'web-mode)
  :hook (web-mode . (lambda ()
                      (setq web-mode-code-indent-offset 2
                            web-mode-css-indent-offset 2
                            web-mode-markup-indent-offset 2
                            web-mode-enable-auto-indentation nil)
                      (when (string-equal "tsx" (file-name-extension buffer-file-name))
                        (setup-tide-mode)))))

(use-package ace-window
  :bind (("C-x o" . ace-window)))

(use-package proof-general)

(use-package company-coq
  :hook (coq-mode . company-coq-mode))
