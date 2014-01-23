;; This is a config that will aim to be speedy. As in near instant start on an SSD.
;; Might not cut it on a conventional HDD but I guess nobody will use it anyway so there.

(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; Disable almost all GUI elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)


;; add all subdirs to load path.
(let ((default-directory "~/.emacs.d/lisp/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; add themes to load path.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Make things pretty
(require 'appearance)

;; install packages when required.
(require 'setup-package)
;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   (cons 'indent-guide melpa)
   (cons 'ace-jump-mode melpa)
   (cons 'move-text melpa)
   (cons 'visual-regexp-steroids melpa)
   (cons 'smartparens melpa)
   (cons 'auto-complete melpa)
   (cons 'mmm-mode melpa)
   (cons 'fic-mode melpa)
   (cons 'rainbow-delimiters melpa)
   (cons 'jedi melpa)
   (cons 'ido melpa)
   (cons 'flx-ido melpa)
   (cons 'ido-ubiquitous melpa)
   (cons 'highlight-escape-sequences melpa)
   (cons 'smartparens melpa)
   (cons 'yasnippet melpa)
   (cons 'diminish melpa)
   (cons 'multiple-cursors melpa)
   (cons 'rainbow-mode gnu)
   (cons 'rect-mark marmalade)
   (cons 'smart-mode-line melpa)
   (cons 'smex melpa)
   (cons 'diminish melpa)
   (cons 'rect-mark melpa)
   (cons 'auctex gnu)
   (cons 'flycheck melpa)
   (cons 'perspective melpa)
   (cons 'cdlatex marmalade)
   (cons 'android-mode melpa)
   (cons 'magit melpa)
   (cons 'fiplr melpa)))

(when (not package-archive-contents) (package-refresh-contents))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(require 'yasnippet)
(yas-global-mode 1)

;; setup minibuffer stuff.
(require 'setup-ido)
(require 'setup-smex)

;; setup specific modes.
(require 'setup-org)
(require 'setup-latex)
(require 'setup-prog)
(require 'setup-python)

;; setup universal emacs stuff.
(require 'setup-universal)

;; add defuns and whatever.
(require 'defuns)

;; remember to add yasnippet...
(yas-global-mode)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
