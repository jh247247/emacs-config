;; fic-mode for TODO etc.
(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(require 'smartparens)
(require 'smartparens-config)
(add-hook 'prog-mode-hook 'smartparens-mode)

(provide 'setup-prog)
