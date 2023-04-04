;; linux-common.el

;; pinentry for Debian/Ubuntu without window system.
;; pinentry shipped with Debian/Ubuntu disables --allow-emacs-pinentry compile option,
;; so pinentry.el cannot be used. the follwoing script is much INSECURE, but anyway it works on CLI.
;;
;; https://github.com/ecraven/pinentry-emacs
;;
;; when window system available, use gtk version of pinentry.
(unless (window-system)
  (when (version<= "26" emacs-version)
    (require 'epg)
    (setq epg-pinentry-mode 'loopback)))

;; font
(when (window-system)
  (exec-path-from-shell-initialize)
  (create-fontset-from-ascii-font "monospace:pixelsize=24:weight=regular:slant=normal" nil "ricty")
  (set-fontset-font "fontset-ricty" 'unicode
                    "Cica:weight=regular:slant=normal" nil 'append)
  (set-fontset-font "fontset-ricty" '(#x1F000 . #x1F02B) ;; Mahjong tiles
                    "FreeSerif:weight=regular:slant=normal")
  (add-to-list 'default-frame-alist '(font . "fontset-ricty")))

;; use aspell for spell checking
(setq ispell-program-name "aspell")

(use-package vterm
  :ensure t
  :config (setq vterm-max-scrollback 10000))
