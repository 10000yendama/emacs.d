;; linux-common.el

;; font
(when (window-system)
  (create-fontset-from-ascii-font "Ricty:pixelsize=18:weight=regular:slant=normal" nil "ricty")
  (add-to-list 'default-frame-alist '(font . "fontset-ricty"))
)

;; theme
(when (window-system)
  (use-package color-theme-sanityinc-tomorrow
    :config
    (load-theme 'sanityinc-tomorrow-eighties t)))
