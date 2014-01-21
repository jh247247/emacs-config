(require 'android-mode)

;; keep a variable so we know what part of the compilation we are at.
(defvar android-compilation-level 0)

;; a custom function to build, install and run an android app on a physical phone.
;; might need some refactoring later as errors are not handled at all.
(defun android-custom-buinst (buffer string)
  (progn (setq android-compilation-level (1+ android-compilation-level))
	 ;; for the first part of the compilation, make sure we are in android mode.
	 ;; otherwise, do not run at all.
         (when (and (and (boundp 'android-mode) android-mode)
                    (= android-compilation-level 1))
	   (android-ant-debug))
         (when (= android-compilation-level 2) (android-ant-installd))
         (when (= android-compilation-level 3)
           (progn (android-start-app) (setq android-compilation-level 0)))))

;; add the keybinding to the mode map, so nothing leaks into the outside world.
(eval-after-load 'android-mode
  '(define-key android-mode-map (kbd "H-i") (lambda () (interactive)
                                              (android-custom-buinst nil nil))))
;; add our function to the end of the list.
;; these functions execute after the end of a compilation.
(add-to-list 'compilation-finish-functions 'android-custom-buinst)
(provide 'android)
