(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(add-hook 'LaTeX-mode-hook '(lambda ()
                              'turn-on-outline-minor-mode
                              (cdlatex-mode)
                              (define-key LaTeX-mode-map
                                (kbd [tab])
                                'indent-for-tab-command)))
(setq outline-minor-mode-prefix "\C-c\C-o")

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
;;(load "auctex.el" nil t t) ; YEAH, AUCTEX!
;;(load "preview-latex.el" nil t t)
;;(setq-default TeX-master nil) ; Query for master file.

(require 'smartparens-latex)
(add-hook 'LaTeX-mode-hook (lambda ()
                             (local-set-key (kbd "RET")
					     'reindent-then-newline-and-indent)))
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

(provide 'setup-latex)
