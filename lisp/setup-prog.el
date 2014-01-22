;; fic-mode for TODO etc.
(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(require 'smartparens)
(require 'smartparens-config)
(setq sp-highlight-pair-overlay nil)
(add-hook 'prog-mode-hook 'smartparens-mode)

;; auto complete stuff
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(defun auto-complete-mode-maybe ()
  "No maybe for you. Only AC!"
  (unless (minibufferp (current-buffer))
    (auto-complete-mode 1)))

;; yasnippet
(require 'yasnippet)

;; Use only own snippets, do not use bundled ones
(setq yas/snippet-dirs '("~/.emacs.d/snippets"))

;; highlight-escape-sequences
(require 'highlight-escape-sequences)
(add-hook 'prog-mode-hook 'hes-mode)

(require 'whitespace)
;; display only tails of lines longer than 80 columns, tabs and
;; trailing whitespaces
(setq whitespace-line-column 80
      whitespace-style '(face lines-tail))

;; face for long lines' tails
(set-face-attribute 'whitespace-line nil
                    :background "red1"
                    :foreground "yellow"
                    :weight 'bold)

;; face for Tabs
(set-face-attribute 'whitespace-tab nil
                    :background "red1"
                    :foreground "yellow"
                    :weight 'bold)

(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook '(lambda ()
                             (local-set-key (kbd "RET")
                                            'reindent-then-newline-and-indent)))
(add-hook 'c-mode-hook 'hide-ifdef-mode)

;; try out flycheck mode.
(add-hook 'after-init-hook #'global-flycheck-mode)

;; make things easy to compile
(global-set-key (kbd "<f1>") 'recompile)

;; find file in project. That means that the project has to be in a
;; git or svn project.

(global-set-key (kbd "C-x f") 'fiplr-find-file)
(provide 'setup-prog)
