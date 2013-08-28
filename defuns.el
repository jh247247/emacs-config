
;; yank multiple times if given a argument.
(defun multiyank (times) (interactive "p") (loop for i from 1 to times do (yank)))
(global-set-key (kbd "C-y") 'multiyank)

(provide 'defuns)
