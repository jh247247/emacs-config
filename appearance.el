(setq visible-bell t)
(show-paren-mode t)


(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(set-background-color "black")
(set-foreground-color "white")

(provide 'appearance)
