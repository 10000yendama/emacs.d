;; cocoa-emacs-common.el

;; font
(when (window-system)
  (create-fontset-from-ascii-font "Ricty:pixelsize=20:weight=regular:slant=normal" nil "ricty")
  (set-fontset-font "fontset-ricty" 'unicode
                    "Ricty:weight=regular:slant=normal" nil 'append)
  (add-to-list 'default-frame-alist '(font . "fontset-ricty")))

;; time locate (affects Org-mode)
(setq system-time-locale "ja_JP.UTF-8")
