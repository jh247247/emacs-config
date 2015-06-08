;; This is a config that will aim to be speedy. As in near instant start on an SSD.
;; Might not cut it on a conventional HDD but I guess nobody will use
;; it anyway so there.

;; This file is to bootstrap the org elisp that is used to load the
;; more interesting things.

                                        ; First, establish a root directory from which we can locate the org-mode files we need.
;; (setq dotfiles-dir (file-name-directory (or (buffer-file-name)
;;                                          load-file-name)))

;; ; Locate the directory that has the org-mode files
;; (let* ((org-dir (expand-file-name
;;                  "lisp" (expand-file-name
;;                          "org-mode" (expand-file-name
;;                                 dotfiles-dir)))))
;;   (add-to-list 'load-path org-dir)
;;   (require 'org)
;;   (require 'ob))


(require 'package)

;; add extra sources to the package archives
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org". "http://orgmode.org/elpa/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

;; activate all the packages
(package-initialize)

                                        ; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; function to install a given list of packages.
(defun packages-require (&rest pl)
  (dolist (p pl)
    (unless (package-installed-p p)
      (package-install p)))
  (package-initialize)
  (delete-other-windows))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

(packages-require 'grandshell-theme)
(load-theme 'grandshell t)

(if window-system nil
  (set-face-background 'default "color-16"))

(packages-require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "<f7>") 'magit-status)

(packages-require 'smex)

;;; Smex
(autoload 'smex "smex"
  "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
your recently and most frequently used commands.")
;;; This adds a hyphen on space, instead of blocking.
(defadvice smex (around space-inserts-hyphen activate compile)
  (let ((ido-cannot-complete-command
         `(lambda ()
            (interactive)
            (if (string= " " (this-command-keys))
                (insert ?-)
              (funcall ,ido-cannot-complete-command)))))
    ad-do-it))

;;; This adds abbreviation to commands, so that magit-status for
;;; example might become mags
;;; Filters ido-matches setting acronynm matches in front of the results
(defadvice ido-set-matches-1 (after ido-acronym-matches activate)
  (if (> (length ido-text) 1)
      (let ((regex (concat "^" (mapconcat 'char-to-string ido-text "[^-]*-")))
            (acronym-matches (list))
            (remove-regexes '("-menu-")))
        ;; Creating the list of the results to be set as first
        (dolist (item items)
          (if (string-match (concat regex "[^-]*$") item) ;; strict match
              (add-to-list 'acronym-matches item)
            (if (string-match regex item) ;; appending relaxed match
                (add-to-list 'acronym-matches item t))))

        ;; Filtering ad-return-value
        (dolist (to_remove remove-regexes)
          (setq ad-return-value
                (delete-if (lambda (item)
                             (string-match to_remove item))
                           ad-return-value)))

        ;; Creating resulting list
        (setq ad-return-value
              (append acronym-matches
                      ad-return-value))

        (delete-dups ad-return-value)
        (reverse ad-return-value))))

(global-set-key (kbd "M-x") 'smex)

(packages-require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10
      ido-case-fold t)

(defun sd/ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match))

(add-hook
 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       (cond
        ((looking-back "~/") (insert "projects/"))
        ((looking-back "/") (insert "~/"))
        (:else (call-interactively 'self-insert-command)))))

   ;; Use C-w to go back up a dir to better match normal usage of C-w
   ;; - insert current file name with C-x C-w instead.
   (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
   (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)))

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

;;; save this bit for later.
;;(ido-ubiquitous-use-new-completing-read webjump 'webjump)
;;(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
;;(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

(packages-require 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)

;; use built-in snippets as well as custom snippets (when I make them)
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.emacs.d/snippets")))

;; auto complete stuff
(packages-require 'auto-complete 'ac-math 'ac-dabbrev)

(require 'auto-complete-config)
(require 'ac-math)

(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

(ac-flyspell-workaround)
(ac-linum-workaround)
(global-auto-complete-mode t)
(setq ac-auto-start 3)
(setq ac-dwim t)
(setq ac-use-fuzzy t)

(set-default 'ac-sources '(ac-source-yasnippet ac-source-dabbrev ac-source-semantic))

(add-to-list 'ac-modes 'latex-mode)
(add-to-list 'ac-modes 'org-mode)

;; maths-y stuff for modes that support it.
(defun ac-latex-setup ()
  (setq ac-sources (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                           ac-sources)))

(add-hook 'LaTeX-mode-hook 'ac-latex-setup)
(add-hook 'org-mode-hook 'ac-latex-setup)

(defun auto-complete-mode-maybe ()
  "No maybe for you. Only AC!"
  (unless (minibufferp (current-buffer))
    (auto-complete-mode 1)))

(define-key ac-complete-mode-map [tab] 'ac-expand)

(packages-require 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode)

(setq-default auto-fill-function 'do-auto-fill)
(set-fill-column 80)
(add-hook 'prog-mode '(lambda () (interactive)
                        (setq-local
                         comment-auto-fill-only-comments t)))

(setq auto-save-interval 300)

;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((latex . t)
   (plantuml . t)
   (sh . t)
   (perl . t)
   (octave . t)))

;; fontify code in code blocks
(setq org-src-fontify-natively t)

;; Don't ask when executing code, idc
(setq org-confirm-babel-evaluate nil)

(packages-require 'plantuml-mode)

(setq org-plantuml-jar-path
      (expand-file-name "/usr/share/plantuml/plantuml.jar"))

(packages-require 'org-ac)
(require 'org-ac)
(org-ac/config-default)

(add-hook 'org-mode-hook #'auto-capitalize-mode)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; set it to some handy key binding.
(global-set-key (kbd "<f3>") 'iwb)

;; sometimes I work with people that indent terribly.
;; for shiggles, lets fix that automatically.
;; note that this is a bit more 'nice' when working in a repo, so
;; kinda misses the point but still useful nonetheless.
;; (setq auto-indent-on-visit-file t)

(packages-require 'aggressive-indent)
(add-hook 'prog-mode-hook (lambda () (aggressive-indent-mode)))
(add-hook 'org-mode-hook (lambda () (aggressive-indent-mode)))

(defun yank-repeat (arg)
  "With numerical ARG, repeat last yank ARG times. "
  (interactive "p*")
  (dotimes (i arg)
    (insert (car kill-ring))))
(define-key global-map (kbd "C-x C-y") 'yank-repeat)

(packages-require 'smart-mode-line)
;;(setq sml/theme 'dark)
(sml/setup)

(packages-require 'ace-jump-mode)
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(setq ace-jump-mode-scope 'frame)

;; hack so that this works in org-mode too.
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "\C-c SPC") 'ace-jump-mode)))

;;If you also use viper mode:
;; maybe one day...
                                        ;(define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)

(require 'saveplace)
(setq-default save-place t)

(packages-require 'ws-butler)
(ws-butler-global-mode)
(setq-default show-trailing-whitespace t)

(global-set-key [(f5)] 'compile-again)
(global-set-key [(f6)] 'next-error)

(setq compilation-last-buffer nil)
(defun compile-again (pfx)
  (interactive "p")
  (if (and (eq pfx 1)
           compilation-last-buffer)
      (progn
        (set-buffer compilation-last-buffer)
        (revert-buffer t t))
    (call-interactively 'compile)))

;; some compilation stuff so that it scrolls to the first error when
;; it happens
(setq compilation-scroll-output 'first-error)

;; require winner mode for the auto closing of the compilation buffer.
(winner-mode 1)

;;(setq compilation-finish-functions 'compile-autoclose)
;; Close the compilation window if there was no error at all.
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
         (bury-buffer "*compilation*")
         (winner-undo)
         (message "Build successful."))
        (t
         (message "Compilation exited abnormally: %s" string))))

(dolist (hook '(org-mode-hook text-mode latex-mode))
  (add-hook hook (lambda () (flyspell-mode 1))))

(packages-require 'auto-capitalize)
(require 'auto-capitalize)

(packages-require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(packages-require 'hungry-delete)
(require 'hungry-delete)
(global-hungry-delete-mode)

(define-key ctl-x-map "\C-i" 'endless/ispell-word-then-abbrev)

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
    (call-interactively 'ispell-word)
    (setq aft (downcase (or (thing-at-point 'word) "")))
    (unless (string= aft bef)
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table)
        bef aft))))

(setq save-abbrevs t)
(setq-default abbrev-mode t)

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

(packages-require 'rect-mark)
(require 'rect-mark)
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x")   'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w")   'rm-kill-region)
(global-set-key (kbd "C-x r M-w")   'rm-kill-ring-save)

(packages-require 'rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'rainbow-delimiters-mode)

(packages-require 'god-mode)
(require 'god-mode)

;; need to set this in console mode only or something...
(if (eq window-system 'nil)  (global-set-key [(f2)] 'god-mode-all))
(global-set-key (kbd "<escape>") 'god-mode-all)

(packages-require 'epa)
(require 'epa)
(when (require 'epa-file nil 'noerror)
  (epa-file-enable)

  ;; t      to always ask for user
  ;; nil    to ask for users unless specified
  ;;'silent to use symmetric encryption:
  (setq epa-file-select-key 'silent)

  ;;Note: if you have an instance of seahorse running, then the environment
  ;;variable GPG_AGENT_INFO=/tmp/seahorse-nDQm50/S.gpg-agent:6321:1, which
  ;;causes emacs to start a GUI for password, instead of using mini-buffer.

  (setenv "GPG_AGENT_INFO" nil)
  ;; Note: another form is:
  ;;(setenv (concat "GPG_AGENT_INFO" nil))
  )

(packages-require 'multiple-cursors)
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-;") 'mc/mark-all-symbols-like-this-in-defun)

(add-hook 'prog-mode 'subword-mode)

(packages-require 'flycheck 'flycheck-pos-tip)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; show errors in a popup
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(require 'semantic)
(require 'semantic/bovine/gcc)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes
             'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes
             'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes
             'global-semantic-idle-summary-mode)


;; Semantic
(global-semantic-idle-scheduler-mode)
(global-semantic-idle-completions-mode)
(global-semantic-decoration-mode)
(global-semantic-highlight-func-mode)
(global-semantic-show-unmatched-syntax-mode)
(setq semantic-load-turn-useful-things-on t)
(setq global-semantic-idle-local-symbol-highlight-mode t)

(semantic-mode 1)
(global-ede-mode t)
(ede-enable-generic-projects)

;; (global-highlight-changes-mode t)

;; (add-hook 'after-save-hook ((lambda () (interactive)
;;                               (highlight-changes-mode 0)
;;                               (highlight-changes-mode 1))))

(global-set-key (kbd "C-x SPC") 'set-mark-command)
(put 'narrow-to-region 'disabled nil)
