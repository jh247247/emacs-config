(setq visible-bell t)
(show-paren-mode t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))


;; load one of the only dark high contrast themes with a sane color scheme.
(require 'color-theme)
(color-theme-initialize)
(color-theme-billw)


(provide 'appearance)
