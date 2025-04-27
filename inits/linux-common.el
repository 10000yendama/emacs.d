;; linux-common.el

;; font
(when (window-system)
  (exec-path-from-shell-initialize)
  (create-fontset-from-ascii-font "PlemolJP:weight=regular:slant=normal" nil "ricty")
  (set-fontset-font "fontset-ricty" 'unicode
                    "PlemolJP:weight=regular:slant=normal" nil 'append)
  ;; Set fonts for symbols
  (set-fontset-font "fontset-ricty" 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font "fontset-ricty" 'symbol "Noto Sans Symbols2" nil 'append)
  (set-fontset-font "fontset-ricty" 'symbol "Noto Sans" nil 'append)
  (set-fontset-font "fontset-ricty" 'symbol "Noto Sans Math" nil 'append)
  (set-fontset-font "fontset-ricty" 'symbol "Noto Sans Symbols" nil 'append)
  (set-fontset-font "fontset-ricty" '(#x1F000 . #x1F02B) ;; Mahjong tiles
                    "FreeSerif:weight=regular:slant=normal")
  (add-to-list 'default-frame-alist '(font . "fontset-ricty")))

;; use aspell for spell checking
(setq ispell-program-name "aspell")

(use-package read-aloud
  :straight nil
  :config
  (setq read-aloud-engines
        '("voicevox"
          (cmd "bash" args ("~/.local/bin/voicevox_wrapper.sh"))))
  (setq read-aloud-engine "voicevox"))
