;; This is a config that will aim to be speedy. As in near instant start on an SSD.
;; Might not cut it on a conventional HDD but I guess nobody will use
;; it anyway so there.

;; This file is to bootstrap the org elisp that is used to load the
;; more interesting things.

; First, establish a root directory from which we can locate the org-mode files we need.
(setq dotfiles-dir (file-name-directory (or (buffer-file-name)
					    load-file-name)))

; Locate the directory that has the org-mode files
(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org-mode" (expand-file-name
                                dotfiles-dir)))))
  (add-to-list 'load-path org-dir)
  (require 'org)
  (require 'ob))

(mapc #'org-babel-load-file (directory-files dotfiles-dir t "emacs.org$"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(magit-diff-options nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
