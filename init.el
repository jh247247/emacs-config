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
