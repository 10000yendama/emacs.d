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

;; hide menu-bar
(menu-bar-mode -1)

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

;; Use loopback Pinentry
(setq epg-pinentry-mode 'loopback)

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

(use-package sensitive-mode
  :straight nil)

(use-package org
  :ensure nil
  :hook (org-mode . (lambda ()
                      (auto-fill-mode)
                      (sensitive-mode)))
  :bind (("S-<iso-lefttab>" . org-shifttab))
  :config
  (require 'org-crypt)
  (setq org-tags-exclude-from-inheritance (quote ("crypt"))
        org-crypt-key "7E2A16BCD09D9078")
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
  :bind (("C-x o" . ace-window))
  :config
  ;; ignore *Zunda* buffer from ace-window's buffer selection
  (push "*Zunda*" aw-ignored-buffers))

(use-package proof-general)

(use-package company-coq
  :hook (coq-mode . company-coq-mode))

(use-package zunda-mode
  :straight nil ; local package
  :config
  (setq zunda-psd-files
        `(((filename . "~/Documents/zunda/ずんだもん立ち絵1.psd")
           (additional-rules
            . ((layer-dependencies . (((layers . (("*01.アイドル1")))
                                       (depends-on . (("*01.アイドル1" "眉毛") ("*01.アイドル1" "目") ("*01.アイドル1" "口"))))
                                      ((layers . (("*02.アイドリング")))
                                       (depends-on . (("*02.アイドリング" "眉") ("*02.アイドリング" "目") ("*02.アイドリング" "口"))))
                                      ((layers . (("*03.腰に手")))
                                       (depends-on . (("*03.腰に手" "眉") ("*03.腰に手" "目") ("*03.腰に手" "口"))))
                                      ((layers . (("*04.煽りっぽい")))
                                       (depends-on . (("*04.煽りっぽい" "眉") ("*04.煽りっぽい" "目") ("*04.煽りっぽい" "口"))))
                                      ((layers . (("*05.指差し")))
                                       (depends-on . (("*05.指差し" "眉") ("*05.指差し" "目") ("*05.指差し" "口"))))
                                      ((layers . (("*06.喜び")))
                                       (depends-on . (("*06.喜び" "眉") ("*06.喜び" "目") ("*06.喜び" "口") ("*06.喜び" "06.喜びベース"))))
                                      ((layers . (("*07.悩む")))
                                       (depends-on . (("*07.悩む" "眉") ("*07.悩む" "目") ("*07.悩む" "口"))))
                                      ((layers . (("*08.腕組")))
                                       (depends-on . (("*08.腕組" "眉") ("*08.腕組" "目") ("*08.腕組" "口"))))
                                      ((layers . (("*09.片手指差し")))
                                       (depends-on . (("*09.片手指差し" "眉") ("*09.片手指差し" "目") ("*09.片手指差し" "口"))))
                                      ((layers . (("*10.片手上げ")))
                                       (depends-on . (("*10.片手上げ" "眉") ("*10.片手上げ" "目") ("*10.片手上げ" "口"))))
                                      ((layers . (("*11.やーれやれ")))
                                       (depends-on . (("*11.やーれやれ" "眉") ("*11.やーれやれ" "目") ("*11.やーれやれ" "口"))))
                                      ((layers . (("*12.サムズアップ")))
                                       (depends-on . (("*12.サムズアップ" "眉") ("*12.サムズアップ" "目") ("*12.サムズアップ" "口"))))))))
           (animation-steps . (((duration . 0.15)
                                (layer-dependencies . (((layers . (("*01.アイドル1"))) (depends-on . (("*01.アイドル1" "目" "*閉"))))
                                                       ((layers . (("*02.アイドリング"))) (depends-on . (("*02.アイドリング" "目" "*閉"))))
                                                       ((layers . (("*03.腰に手"))) (depends-on . (("*03.腰に手" "目" "*閉"))))
                                                       ((layers . (("*04.煽りっぽい"))) (depends-on . (("*04.煽りっぽい" "目" "*閉"))))
                                                       ((layers . (("*05.指差し"))) (depends-on . (("*05.指差し" "目" "*閉"))))
                                                       ((layers . (("*06.喜び"))) (depends-on . (("*06.喜び" "目" "*閉"))))
                                                       ((layers . (("*07.悩む"))) (depends-on . (("*07.悩む" "目" "*閉"))))
                                                       ((layers . (("*08.腕組"))) (depends-on . (("*08.腕組" "目" "*閉"))))
                                                       ((layers . (("*09.片手指差し"))) (depends-on . (("*09.片手指差し" "目" "*閉"))))
                                                       ((layers . (("*10.片手上げ"))) (depends-on . (("*10.片手上げ" "目" "*閉"))))
                                                       ((layers . (("*11.やーれやれ"))) (depends-on . (("*11.やーれやれ" "目" "*閉"))))
                                                       ((layers . (("*12.サムズアップ"))) (depends-on . (("*12.サムズアップ" "目" "*閉")))))))
                               ((duration . ,(let ((gen (zunda-box-muller-generator))
                                                   (sigma 1)
                                                   (mu 8))
                                               (lambda () (+ (* sigma (funcall gen)) mu))))
                                (layer-dependencies . nil)))))
          ((filename . "~/Documents/zunda/ずんだもん立ち絵2.psd")
           (additional-rules
            . ((layer-dependencies . (((layers . (("*13.ダブルピース")))
                                       (depends-on . (("*13.ダブルピース" "眉") ("*13.ダブルピース" "目") ("*13.ダブルピース" "口"))))
                                      ((layers . (("*14.怯え")))
                                       (depends-on . (("*14.怯え" "眉") ("*14.怯え" "目") ("*14.怯え" "口"))))
                                      ((layers . (("*15.やったー")))
                                       (depends-on . (("*15.やったー" "眉") ("*15.やったー" "目") ("*15.やったー" "口"))))
                                      ((layers . (("*16.怒る")))
                                       (depends-on . (("*16.怒る" "眉") ("*16.怒る" "目") ("*16.怒る" "口"))))
                                      ((layers . (("*17.驚く")))
                                       (depends-on . (("*17.驚く" "眉") ("*17.驚く" "目") ("*17.驚く" "口"))))
                                      ((layers . (("*18.ふらふら")))
                                       (depends-on . (("*18.ふらふら" "眉") ("*18.ふらふら" "目") ("*18.ふらふら" "口"))))
                                      ((layers . (("*19.It's me")))
                                       (depends-on . (("*19.It's me" "眉") ("*19.It's me" "目") ("*19.It's me" "口"))))
                                      ((layers . (("*20.ファイティン1")))
                                       (depends-on . (("*20.ファイティン1" "眉") ("*20.ファイティン1" "目") ("*20.ファイティン1" "口"))))
                                      ((layers . (("*21.ファイティン2")))
                                       (depends-on . (("*21.ファイティン2" "眉") ("*21.ファイティン2" "目") ("*21.ファイティン2" "口"))))
                                      ((layers . (("*22.Fxxk")))
                                       (depends-on . (("*22.Fxxk" "眉") ("*22.Fxxk" "目") ("*22.Fxxk" "口"))))
                                      ((layers . (("*23.媚")))
                                       (depends-on . (("*23.媚" "眉") ("*23.媚" "目") ("*23.媚" "口"))))
                                      ((layers . (("*24.ダメージ")))
                                       (depends-on . (("*24.ダメージ" "眉") ("*24.ダメージ" "目") ("*24.ダメージ" "口"))))))))
           (animation-steps . (((duration . 0.15)
                                (layer-dependencies . (((layers . (("*13.ダブルピース"))) (depends-on . (("*13.ダブルピース" "目" "*閉"))))
                                                       ((layers . (("*14.怯え"))) (depends-on . (("*14.怯え" "目" "*閉"))))
                                                       ((layers . (("*15.やったー"))) (depends-on . (("*15.やったー" "目" "*閉"))))
                                                       ((layers . (("*16.怒る"))) (depends-on . (("*16.怒る" "目" "*閉"))))
                                                       ((layers . (("*17.驚く"))) (depends-on . (("*17.驚く" "目" "*閉"))))
                                                       ((layers . (("*18.ふらふら"))) (depends-on . (("*18.ふらふら" "目" "*閉"))))
                                                       ((layers . (("*19.It's me"))) (depends-on . (("*19.It's me" "目" "*閉"))))
                                                       ((layers . (("*20.ファイティン1"))) (depends-on . (("*20.ファイティン1" "目" "*閉"))))
                                                       ((layers . (("*21.ファイティン2"))) (depends-on . (("*21.ファイティン2" "目" "*閉"))))
                                                       ((layers . (("*22.Fxxk"))) (depends-on . (("*22.Fxxk" "目" "*閉"))))
                                                       ((layers . (("*23.媚"))) (depends-on . (("*23.媚" "目" "*閉"))))
                                                       ((layers . (("*24.ダメージ"))) (depends-on . (("*24.ダメージ" "目" "*閉")))))))
                               ((duration . ,(let ((gen (zunda-box-muller-generator))
                                                   (sigma 1)
                                                   (mu 8))
                                               (lambda () (+ (* sigma (funcall gen)) mu))))
                                (layer-dependencies . nil)))))
          ((filename . "~/Documents/zunda/ずんだもん立ち絵素材V3.2_基本版.psd")
           (additional-rules . ((forbidden-layers . (("!右腕" "*(非表示)")))
                                (layer-dependencies . (((layers . (("!左腕" "*腕組み(右腕は非表示に)")))
                                                        (depends-on . (("!右腕" "*(非表示)"))))))))
           (animation-steps . (((duration . 0.15)
                                (layer-dependencies . (((layers . nil)
                                                        (depends-on . (("!目" "*閉じ目")))))))
                               ((duration . ,(let ((gen (zunda-box-muller-generator))
                                                   (sigma 1)
                                                   (mu 8))
                                               (lambda () (+ (* sigma (funcall gen)) mu))))
                                (layer-dependencies . nil)))))
          ((filename . "~/Documents/zunda/ずんだもん立ち絵素材改ver1.1.1.psd")
           (additional-rules . ((forbidden-layers . (("!右腕" "*(非表示)")))
                                (layer-dependencies . (((layers . (("!左腕" "*腕組み(右腕は非表示に)")))
                                                        (depends-on . (("!右腕" "*(非表示)"))))
                                                       ((layers . (("!右腕" "*基本(直立用)")))
                                                        (depends-on . (("!体" "*体直立"))))
                                                       ((layers . (("!右腕" "*ポケット(直立用)")))
                                                        (depends-on . (("!体" "*体直立"))))
                                                       ((layers . (("!右腕" "*ポケット(前傾用)")))
                                                        (depends-on . (("!体" "*体前傾"))))))))
           (animation-steps . (((duration . 0.15)
                                (layer-dependencies . (((layers . (("*頭_正面向き")))
                                                        (depends-on . (("*頭_正面向き" "!目" "*なごみ目"))))
                                                       ((layers . (("*頭_上向き")))
                                                        (depends-on . (("*頭_上向き" "!目" "*なごみ目")))))))
                               ((duration . ,(let ((gen (zunda-box-muller-generator))
                                                   (sigma 1)
                                                   (mu 8))
                                               (lambda () (+ (* sigma (funcall gen)) mu))))
                                (layer-dependencies . nil)))))
          ((filename . "~/Documents/zunda/ずんだもん立ち絵素材2.3.psd")
           (additional-rules . ((forbidden-layers . (("パーカー裏地")
                                                     ("*服装1" "!右腕" "*(非表示)")
                                                     ("*服装1" "!左腕" "*(非表示)")
                                                     ("*服装2" "!右腕" "*(非表示)")
                                                     ("*服装2" "!左腕" "*(非表示)")
                                                     ("*服装2" "*素体")
                                                     ("*服装2" "*ぱんつ")
                                                     ("!口" "*おほお")
                                                     ("!口" "*はへえ")
                                                     ("!顔色" "*青ざめ")))
                                (layer-dependencies . (((layers . nil)
                                                        (depends-on . (("尻尾的なアレ"))))  ;; always visible
                                                       ((layers . (("!枝豆" "*パーカー(裏地とセットで使用)") ("*服装1")))
                                                        (depends-on . (("パーカー裏地")
                                                                       ("*服装1" "!右腕" "*(非表示)")
                                                                       ("*服装1" "!左腕" "*(非表示)"))))
                                                       ((layers . (("!枝豆" "*パーカー(裏地とセットで使用)") ("*服装2")))
                                                        (depends-on . (("パーカー裏地")
                                                                       ("*服装2" "!右腕" "*(非表示)")
                                                                       ("*服装2" "!左腕" "*(非表示)"))))))))
           (animation-steps . (((duration . 0.15)
                                (layer-dependencies . (((layers . nil) (depends-on . (("!目" "*なごみ目")))))))
                               ((duration . ,(let ((gen (zunda-box-muller-generator))
                                                   (sigma 1)
                                                   (mu 8))
                                               (lambda () (+ (* sigma (funcall gen)) mu))))
                                (layer-dependencies . nil))))))))

(provide '20-common)
;;; 20-common.el ends here
