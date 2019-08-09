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
(load custom-file)

;; emacs のサーバーを起動。emacsclientコマンドでemacs serverを利用した
;; 編集が可能になる。

;; たとえば、emacsを常に起動している状況を考える。端末等で作業している
;; とき、バージョン管理のコミットログとかで、emacsを使って編集しようと
;; する。このとき、emacsをEDITORに指定すると、確かにemacsが起動するが、
;; 別のemacsが新しく起動する。すでに起動しているのに。そこで
;; emacsclientの出番。EDITOR=emacsclientとすると、すでにそのマシンで起
;; 動しているemacsのバッファとして開いてくれる。編集を終了するときは
;; C-x #とする。これでバッファが閉じられ、もとのコマンド（gitとか）に
;; 結果が渡される。Windowsでもこれは有用。例えばエクスプローラで適当
;; な.txtを開くとき、テキストエディタとしてemacsclientを使うよう関連付
;; けておく。すると、すでにそのマシンで起動しているemacsの新しいバッファ
;; として開くことができ、emacsを複数起動することを避けられる。
(unless (server-running-p)
  (server-start))

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
