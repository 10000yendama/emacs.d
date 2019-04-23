;; -*- coding: utf-8 -*-

;; emacs -q -l ~/path/to/init.el としたとき、~/path/to を
;; user-emacs-directory とする。これにより環境の隔離が簡単になる。
;;
;; なお、以降設定ファイルでは user-emacs-directory の値を直接記述
;; しないようにする。 ~/.emacs.d/path/to/file.el のようなパスは、
;; (locate-user-emacs-file "path/to/file.el") のように記述する。
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; custom で定義される変数を書いておくファイル
;; 基本的にcustomは使わずに、
;;   1) パッケージの設定は use-package の仕組みで行う
;;   2) Emacs標準機能の設定は分割したinitファイルで行う
;; ただし、customを使ったときにinit.elが荒れないように対策として書いておく。
(setq custom-file (locate-user-emacs-file "custom.el")) 

;; パッケージマネージャ straight.el が無ければインストールする
;; Reference: https://github.com/raxod502/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; パッケージの設定に便利なマクロ use-package
(straight-use-package 'use-package)
;; use-package したとき、パッケージがインストールされていなければ
;; straight.el でインストール
(setq straight-use-package-by-default t)

;; init-loader
(use-package init-loader
  :config
  (init-loader-load (locate-user-emacs-file "inits")))

;; init-loader will load files under ~/.emacs.d/inits.
