;; fic-mode for TODO etc.
(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

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

;; auto import defines functions and stuff from header files
(require 'ac-c-headers)
(add-hook 'c-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

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

(defun reindent-sans-python () (interactive)
       (if (eq major-mode 'python-mode)
           (newline)
         (reindent-then-newline-and-indent)))
(add-hook 'prog-mode-hook '(lambda ()
                             (local-set-key (kbd "RET")
                                            'reindent-sans-python)))
(add-hook 'c-mode-hook 'hide-ifdef-mode)

;; try out flycheck mode.
(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck-tip)
(add-hook 'prog-mode-hook '(lambda ()
                             (local-set-key (kbd "C-c C-n")
                                            'flycheck-tip-cycle)))
;; make things easy to compile
(global-set-key (kbd "<f1>") 'recompile)

;; find file in project. That means that the project has to be in a
;; git or svn project.

(global-set-key (kbd "C-x f") 'fiplr-find-file)

(defun kill-and-join-forward (&optional arg)
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1)
             (kill-line arg))
    (kill-line arg)))

(global-set-key "\C-k" 'kill-and-join-forward)

(setq compilation-scroll-output 'first-error)

;; add rainbow idents, yay
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

(provide 'setup-prog)
