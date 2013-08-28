
;; yank multiple times if given a argument.
(defun yank-repeat (arg)
   "With numerical ARG, repeat last yank ARG times. "
   (interactive "p*")
   (dotimes (i arg)
     (insert (car kill-ring))))
(global-unset-key (kbd "C-y"))
(define-key global-map (kbd "C-y") 'yank-repeat)

(provide 'defuns)
