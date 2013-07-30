;; make it so that doctrings are pretty.
(require 'mmm-auto)

(setq mmm-global-mode 'auto
      mmm-submode-decoration-level 2
      mmm-parse-when-idle t)

(mmm-add-classes
 '((python-rst
    :submode rst-mode
    :front "^ *[ru]?\"\"\"[^\"]*$"
    :back "\"\"\"$"
    :include-front t
    :include-back t
    :end-not-begin t)))

(mmm-add-mode-ext-class 'python-mode nil 'python-rst)

(add-hook 'python-mode-hook 'jedi:setup)
(provide 'setup-python)
