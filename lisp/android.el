(require 'android-mode)

(defvar android-compilation-level 0)

(defun android-custom-buinst (buffer string)
  (progn (setq android-compilation-level (1+ android-compilation-level))
         (when (and (and (boundp 'android-mode) android-mode)
                    (= android-compilation-level 1))
	   (android-ant-debug))
         (when (= android-compilation-level 2) (android-ant-installd))
         (when (= android-compilation-level 3)
           (progn (android-start-app) (setq android-compilation-level 0)))))

(eval-after-load 'android-mode
  '(define-key android-mode-map (kbd "H-i") (lambda () (interactive)
                                              (android-custom-buinst nil nil))))

(add-to-list 'compilation-finish-functions 'android-custom-buinst)
(provide 'android)
